use std::os::fd::{AsRawFd, OwnedFd};

use anyhow::Context;
use log::debug;
use nix::unistd::ForkResult;

macro_rules! cstr {
    ($lit:literal) => {
        std::ffi::CStr::from_bytes_with_nul($lit.as_bytes()).expect("invalid CStr literal")
    };
    ( $( $lit:literal ), + ) => {
        &[
            $(
                std::ffi::CStr::from_bytes_with_nul($lit.as_bytes()).expect("invalid CStr literal"),
             )+
        ]
    };
}

pub fn create_pty() -> anyhow::Result<OwnedFd> {
    let winsize = None;
    let termios = None;
    let res = unsafe { nix::pty::forkpty(winsize, termios).context("forkpty failed")? };
    match res.fork_result {
        ForkResult::Parent { child } => {
            debug!("pty: child pid: {child}");
            Ok(res.master)
        }
        ForkResult::Child => {
            let _ = nix::unistd::execve(
                cstr!["/bin/zsh\x00"],
                cstr!["/bin/zsh\x00", "--login\x00"],
                cstr![
                    "TERM=rterm\x00",
                    "TERMINFO=target/terminfo\x00",
                    "RRAVI_TEST1=1\x00",
                    "RRAVI_TEST2=1\x00"
                ],
            )
            .expect("execve failed");
            unreachable!();
        }
    }
}

nix::ioctl_read_bad!(tiocgwinsz, libc::TIOCGWINSZ, nix::pty::Winsize);
nix::ioctl_write_ptr_bad!(tiocswinsz, libc::TIOCSWINSZ, nix::pty::Winsize);

pub fn update_pty_window_size(fd: impl AsRawFd, winsz: &nix::pty::Winsize) -> anyhow::Result<()> {
    match unsafe { tiocswinsz(fd.as_raw_fd(), winsz as *const _) } {
        Ok(_) => Ok(()),
        Err(err) => Err(anyhow::format_err!("tiocswinsz failed: {err}")),
    }
}

pub fn compute_winsize(
    rect_width: f32,
    rect_height: f32,
    char_width: f32,
    char_height: f32,
) -> nix::pty::Winsize {
    let num_chars_x = rect_width / char_width;
    let num_chars_y = rect_height / char_height;
    nix::pty::Winsize {
        ws_row: num_chars_y.floor() as u16,
        ws_col: num_chars_x.floor() as u16,
        ws_xpixel: rect_width.floor() as u16,
        ws_ypixel: rect_height.floor() as u16,
    }
}
