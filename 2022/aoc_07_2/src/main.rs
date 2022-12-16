use std::{cell::RefCell, collections::HashMap, io::BufRead, rc::Rc};

#[derive(Debug)]
enum ParseEntry {
    Directory(HashMap<String, Rc<RefCell<ParseEntry>>>),
    File(usize),
}

#[derive(Debug)]
enum Entry {
    Directory(HashMap<String, Box<Entry>>),
    File(usize),
}

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();
    let root = Rc::new(RefCell::new(ParseEntry::Directory(HashMap::new())));
    let mut cur_dir = root.clone();
    let mut dir_stack = Vec::new();
    args.iter()
        .enumerate()
        .flat_map(|(i, x)| x.strip_prefix("$ ").map(|x| (i, x)))
        .for_each(|(i, x)| match &x[..2] {
            "cd" => {
                let arg = &x[3..];
                match arg {
                    "/" => {
                        cur_dir = root.clone();
                        dir_stack.clear();
                    }
                    ".." => {
                        cur_dir = dir_stack.pop().unwrap();
                    }
                    e => {
                        dir_stack.push(cur_dir.clone());
                        cur_dir = match &*cur_dir.clone().borrow() {
                            ParseEntry::Directory(hm) => hm.get(e).unwrap().clone(),
                            _ => panic!(),
                        }
                    }
                }
            }
            "ls" => {
                let cur_dir_mem = &mut *cur_dir.borrow_mut();
                let map = match cur_dir_mem {
                    ParseEntry::Directory(hm) => hm,
                    _ => panic!(),
                };
                args.iter()
                    .skip(i + 1)
                    .take_while(|&x| &x[..1] != "$")
                    .for_each(|x| {
                        let (op, name) = x.split_once(' ').unwrap();
                        map.insert(
                            name.to_string(),
                            if op == "dir" {
                                Rc::new(RefCell::new(ParseEntry::Directory(HashMap::new())))
                            } else {
                                Rc::new(RefCell::new(ParseEntry::File(
                                    op.parse::<usize>().unwrap(),
                                )))
                            },
                        );
                    });
            }
            _ => panic!("unknown command"),
        });

    fn unwrap_parse_entry(entry: Rc<RefCell<ParseEntry>>) -> Entry {
        match entry.replace(ParseEntry::File(0)) {
            ParseEntry::File(s) => Entry::File(s),
            ParseEntry::Directory(hm) => Entry::Directory(
                hm.into_iter()
                    .map(|(name, entry)| (name, Box::new(unwrap_parse_entry(entry))))
                    .collect::<HashMap<String, Box<Entry>>>(),
            ),
        }
    }

    let root = unwrap_parse_entry(root);

    let mut sizes = Vec::new();

    fn compute_size(entry: &Entry, sizes: &mut Vec<usize>) -> usize {
        match entry {
            Entry::File(s) => *s,
            Entry::Directory(hm) => {
                let size = hm.iter().map(|(_, v)| compute_size(v, sizes)).sum();
                sizes.push(size);
                size
            }
        }
    }

    let free_space = 70_000_000 - compute_size(&root, &mut sizes);

    println!(
        "{:?}",
        sizes
            .iter()
            .zip(sizes.iter())
            .flat_map(|(x, r_x)| (free_space + x).checked_sub(30_000_000).map(|x| (x, r_x)))
            .min_by_key(|x| x.0)
            .unwrap()
            .1
    );
}
