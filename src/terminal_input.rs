use std::{
    collections::VecDeque,
    ops::Neg,
    os::fd::{AsRawFd, OwnedFd},
    sync::{mpsc, Arc},
};

use egui::mutex::RwLock;

use crate::{
    ansi::{self, AnsiToken},
    grid::{Grid, GridStack},
    pty,
    terminal_emulator::SgrState,
};

#[derive(Debug)]
pub struct ChildProcess {
    _io_thread: std::thread::JoinHandle<()>,
    pub pty_fd: Arc<OwnedFd>,
    pub token_stream: mpsc::Receiver<VecDeque<ansi::AnsiToken>>,
}

impl ChildProcess {
    pub fn spawn(ctx: egui::Context, grids: Arc<RwLock<GridStack>>) -> Self {
        let pty_fd = pty::create_pty().expect("create_pty failed");
        let pty_fd = Arc::new(pty_fd);
        let (tx, rx) = mpsc::channel();

        let io_fd = Arc::clone(&pty_fd);
        let _io_thread = std::thread::Builder::new()
            .name("rterm i/o thread".to_string())
            .spawn(move || {
                input_loop(ctx, io_fd, tx, grids);
            })
            .expect("Failed to spawn input_loop thread");

        Self {
            _io_thread,
            pty_fd,
            token_stream: rx,
        }
    }
}

pub fn ctrl(key_name: &str, dst: &mut String) {
    match key_name.as_bytes() {
        [] => (),
        [first_byte @ 0x40..=0x7f] => append_ctrl_masked_key(*first_byte, dst),
        b"Backslash" | b"OpenBracket" | b"CloseBracket" => append_ctrl_masked_key(b'\\', dst),
        _unsupported => log::warn!("Unsupported ctrl key name: {}", key_name),
    }
}

// See: https://pbxbook.com/other/ctrlcods.html
// clear the top two bits to get the ctrl code
fn append_ctrl_masked_key(ascii_key: u8, dst: &mut String) {
    let ctrl_key_code = &[ascii_key & 0b0011_1111];
    let as_str = unsafe { std::str::from_utf8_unchecked(ctrl_key_code) };
    dst.push_str(as_str);
}

pub fn alt(txt: &str, dst: &mut String) {
    dst.push_str(match txt {
        "å" => "\u{1b}a",
        "∫" => "\u{1b}b",
        "ç" => "\u{1b}c",
        "∂" => "\u{1b}d",
        "e" => "\u{1b}e",
        "ƒ" => "\u{1b}f",
        "©" => "\u{1b}g",
        "˙" => "\u{1b}h",
        "i" => "\u{1b}i",
        "∆" => "\u{1b}j",
        "˚" => "\u{1b}k",
        "¬" => "\u{1b}l",
        "µ" => "\u{1b}m",
        "n" => "\u{1b}n",
        "ø" => "\u{1b}o",
        "π" => "\u{1b}p",
        "œ" => "\u{1b}q",
        "®" => "\u{1b}r",
        "ß" => "\u{1b}s",
        "†" => "\u{1b}t",
        "u" => "\u{1b}u",
        "√" => "\u{1b}v",
        "∑" => "\u{1b}w",
        "≈" => "\u{1b}x",
        "¥" => "\u{1b}y",
        "Ω" => "\u{1b}z",
        _ => {
            log::warn!("Unsupported alt key name: {}", txt);
            return;
        }
    });
}

pub fn input_loop(
    ctx: egui::Context,
    pty_fd: Arc<OwnedFd>,
    tx: mpsc::Sender<VecDeque<AnsiToken>>,
    grids: Arc<RwLock<GridStack>>,
) {
    let mut parser = ansi::Parser::new();
    let mut buf = [0u8; 1024];
    loop {
        match nix::unistd::read(pty_fd.as_raw_fd(), &mut buf) {
            Ok(0) => {
                // eof, process exit
                return;
            }
            Ok(num_read) => {
                log::debug!(
                    "read {} bytes from pty: {as_str:?}",
                    num_read,
                    as_str = std::str::from_utf8(&buf[..num_read])
                );
                parser.push_bytes(&buf[..num_read]);
                log::debug!("parsed_tokens: {:?}", &parser.parsed_tokens);

                if parser.has_tokens() {
                    let gui_tokens = {
                        let mut tokens = parser.tokens();
                        // TODO; don't make UI thread wait while we handle grid
                        // tokens, if possible
                        let mut grids = grids.write();
                        tokens.retain(|token| handle_grid_tokens(&mut grids, token) == false);
                        tokens
                    };

                    // even if gui_tokens is empty, send it as a signal that we handled some token
                    // so that GUI do some things like scrolling to the bottom
                    tx.send(gui_tokens)
                        .expect("send to never fail, unless on app shutdown");
                    ctx.request_repaint();
                }
            }
            Err(unexpected) => {
                log::warn!("read failed: {}", unexpected);
                break;
            }
        }
    }
}

fn handle_grid_tokens(grids: &mut GridStack, token: &AnsiToken) -> bool {
    use ansi::{AsciiControl, CursorControl, EraseControl, ModeControl, SgrControl};
    let grid = grids.current_mut();
    match token {
        AnsiToken::ResetToInitialState => {
            grid.clear_including_scrollback();
        }
        AnsiToken::ModeControl(ModeControl::AlternateScreenEnter) => {
            grids.enter_alternate_grid();
        }
        AnsiToken::ModeControl(ModeControl::AlternateScreenExit) => {
            grids.exit_alternate_grid();
        }
        AnsiToken::Text(txt) => {
            grid.write_text_at_cursor(&txt);
        }
        AnsiToken::AsciiControl(AsciiControl::Backspace) => {
            grid.move_cursor_relative(0, -1);
        }
        AnsiToken::AsciiControl(AsciiControl::Tab) => {
            // as advertised in the "it" terminfo entry
            let tab_width = 8;
            let (row, col) = grid.cursor_position();
            let next_tabstop = col + (tab_width - col % tab_width);
            let limit = grid.num_cols() - 1;
            grid.move_cursor(row, next_tabstop.min(limit));
        }
        AnsiToken::AsciiControl(AsciiControl::LineFeed) => {
            grid.insert_linebreak_if_needed();
        }
        AnsiToken::AsciiControl(AsciiControl::CarriageReturn) => {
            let (current_row, _) = grid.cursor_position();
            grid.move_cursor(current_row, 0);
        }
        AnsiToken::CursorControl(CursorControl::MoveUp { lines }) => {
            grid.move_cursor_relative((*lines).try_into().unwrap_or(0_i32).neg(), 0);
        }
        AnsiToken::CursorControl(CursorControl::MoveDown { lines }) => {
            grid.move_cursor_relative((*lines).try_into().unwrap_or(0_i32), 0);
        }
        AnsiToken::CursorControl(CursorControl::MoveLeft { cols }) => {
            grid.move_cursor_relative(0, (*cols).try_into().unwrap_or(0_i32).neg());
        }
        AnsiToken::CursorControl(CursorControl::MoveRight { cols }) => {
            grid.move_cursor_relative(0, (*cols).try_into().unwrap_or(0_i32));
        }
        AnsiToken::CursorControl(CursorControl::MoveTo { line, col }) => {
            grid.move_cursor(line.saturating_sub(1) as u32, col.saturating_sub(1) as u32);
        }
        AnsiToken::CursorControl(CursorControl::ScrollUpFromHome) => {
            let (cursor_row, _) = grid.cursor_position();
            if cursor_row > 0 {
                // no need to scroll
                grid.move_cursor_relative(-1, 0);
            } else {
                grid.move_lines_down(0, 1);
            }
        }
        AnsiToken::CursorControl(CursorControl::SavePositionDEC) => {
            grid.save_cursor_state();
        }
        AnsiToken::CursorControl(CursorControl::RestorePositionDEC) => {
            grid.restore_cursor_state();
        }
        AnsiToken::EraseControl(EraseControl::Screen) => {
            grid.erase_screen();
            // UI need to scroll to the home row, so we don't mark this token has handled at this
            // later
            return false;
        }
        AnsiToken::EraseControl(EraseControl::ScreenAndScrollback) => {
            grid.clear_including_scrollback();
        }
        AnsiToken::EraseControl(EraseControl::FromCursorToEndOfScreen) => {
            grid.erase_from_cursor_to_screen();
        }
        AnsiToken::EraseControl(EraseControl::FromCursorToEndOfLine) => {
            grid.erase_from_cursor_to_eol();
        }
        AnsiToken::SGR(params) => {
            let sgr_state = grid.cursor_format_mut();
            for sgr in params {
                match sgr {
                    SgrControl::Bold => {
                        sgr_state.bold = true;
                    }
                    SgrControl::EnterItalicsMode => {
                        sgr_state.italic = true;
                    }
                    SgrControl::ExitItalicsMode => {
                        sgr_state.italic = false;
                    }
                    SgrControl::ForgroundColor(color) => {
                        sgr_state.fg_color = *color;
                    }
                    SgrControl::BackgroundColor(color) => {
                        sgr_state.bg_color = *color;
                    }
                    SgrControl::Reset => {
                        *sgr_state = SgrState::default();
                    }
                    SgrControl::ResetFgColor => {
                        sgr_state.fg_color = ansi::Color::DefaultFg;
                    }
                    SgrControl::ResetBgColor => {
                        sgr_state.bg_color = ansi::Color::DefaultBg;
                    }
                    SgrControl::Unimplemented(unknown) => {
                        log::warn!("ignoring unimplemented sgr: {unknown:?}");
                    }
                }
            }
        }
        _ => return false,
    };
    true
}
