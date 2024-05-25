use std::sync::{Mutex, RwLock};

// Buffer facilitates non-blocing, thread-safe access to the shared text buffer between the UI
/// thread and the IO thread.
///
/// Because we use a clone-on-write data structure to store the buffer contents, this type can hand
/// out a cheap clone-s of the tree to the UI thread, while the IO thread mutates a separate,
/// clone-on-write copy of the tree. This allows the program to respond to user input like Ctrl-C
/// even the IO thread is busy processing bytes from the pty fd because user input is processed by
/// the UI thread and we take care not to block that thread to get safe access to the text buffer.
///
/// Terminal buffer (aka the Tree) is mainly mutated by reading bytes from the pty fd (i.e the IO
/// thread), but the UI thread might occassionally need to mutate it when the user resizes the
/// window, for example. Because of this, write access to the underlying tree between the UI and IO
/// thread requires coordination.
///
/// This coordination is accomplished by keeping two (cheap) clones of the tree: `snapshot` and
/// `modified`. Threads that want read-only access are given cheap clones of `snapshot` and writers
/// take turns mutating the `modified` copy and then "commit" the modified copy by storing it as
/// the `snapshot`.
#[derive(Debug)]
pub struct Buffer<T> {
    snapshot: RwLock<T>,
    modified: Mutex<T>,
}

impl<T> Buffer<T>
where
    T: Clone,
{
    pub fn new(t: T) -> Self {
        // make a cheap clone
        let modified = Mutex::new(t.clone());
        let snapshot = RwLock::new(t);

        Self { snapshot, modified }
    }

    /// Returns the latest snapshot of the underlying tree. This is a cheap clone-on-write copy and
    /// modifications made by future calls to `write` will not be visible to the returned copy. The
    /// caller is free to modify the returned Tree as they see fit.
    pub fn read(&self) -> T {
        let guard = self.snapshot.read().expect("mutex was poisoned");
        guard.clone()
    }

    /// Allows callers to modify the shared Tree instance safely. The modified tree is committed to
    /// the snapshot and will be visible on next call to read (on the same thread).
    pub fn write<F, U>(&self, mut f: F) -> U
    where
        F: FnMut(&mut T) -> U,
    {
        let mut modified = self.modified.lock().expect("mutex was poisoned");

        // hand out the modified copy for mutation
        let u = f(&mut modified);

        // commit the modified copy for subsequent reads
        let modified = modified.clone();
        let mut commit_guard = self.snapshot.write().expect("rwlock poisoned");
        *commit_guard = modified;
        u
    }
}

#[cfg(test)]
mod tests {
    use crate::{terminal_emulator::SgrState, tree::Tree};

    use super::*;

    #[test]
    fn test_buffer_1() {
        let tree = Tree::new();
        let buf = Buffer::new(tree);

        const N: usize = 100;
        const WRITERS: usize = 2;
        const READERS: usize = 10;

        std::thread::scope(|s| {
            for i in 0..WRITERS {
                let buf = &buf;
                s.spawn(move || {
                    for j in 0..N {
                        let line = format!("line{j} from writer #{i}\n");
                        buf.write(|tree| {
                            tree.push_str(&line, SgrState::default());
                        });
                        std::thread::yield_now();
                    }
                });
            }

            for _i in 0..READERS {
                let buf = &buf;
                s.spawn(move || {
                    let mut sum = 0;
                    for _j in 0..N {
                        let tree = buf.read();
                        let nc = tree.len_chars();
                        // eprintln!("reader #{i}, iter: {j} saw tree with {nc} chars");
                        sum += nc;
                        std::thread::yield_now();
                    }
                    sum
                });
            }
        });

        // after all the writes are done, the number of lines in the tree is deterministic
        assert_eq!(buf.read().len_lines(), WRITERS * N);
    }
}
