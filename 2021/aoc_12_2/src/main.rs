use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::io::BufRead;

#[derive(Debug)]
struct Node<'a> {
    name: &'a str,
    small: bool,
    start_end: bool,
}

impl Node<'_> {
    fn start() -> Self {
        Node {
            name: "start",
            small: true,
            start_end: true,
        }
    }
    fn end() -> Self {
        Node {
            name: "end",
            small: true,
            start_end: true,
        }
    }
}

impl PartialEq for Node<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl Eq for Node<'_> {}

impl Hash for Node<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let links: Vec<(Node, Node)> = args
        .iter()
        .map(|x| {
            let mut s = x.split('-');
            let t = (s.next().unwrap(), s.next().unwrap());
            (
                Node {
                    name: t.0,
                    small: t.0.chars().next().unwrap().is_lowercase(),
                    start_end: t.0 == "start" || t.0 == "end",
                },
                Node {
                    name: t.1,
                    small: t.1.chars().next().unwrap().is_lowercase(),
                    start_end: t.1 == "start" || t.1 == "end",
                },
            )
        })
        .collect();

    let mut graph: HashMap<&Node, Vec<&Node>> = HashMap::new();

    for (n1, n2) in links.iter() {
        graph.entry(n1).or_insert(Vec::new()).push(n2);
        graph.entry(n2).or_insert(Vec::new()).push(n1);
    }

    let mut counter = 0;

    let root = Node::start();
    let mut queue: Vec<(Vec<&Node>, bool)> = Vec::new();
    queue.push((vec![&root], false));
    while queue.len() > 0 {
        let path = queue.pop().unwrap();
        let last = *path.0.last().unwrap();
        if last == &Node::end() {
            counter += 1;
        }
        if let Some(list) = graph.get(last) {
            for n in list {
                let mut twice = path.1;
                if !n.small
                    || (!n.start_end && !path.1 && {
                        let mut iter = path.0.iter();
                        twice = iter.find(|&x| x == n).is_some();
                        iter.find(|&x| x == n).is_none()
                    })
                    || !path.0.contains(n)
                {
                    let mut new_path = path.clone();
                    new_path.0.push(n);
                    new_path.1 = twice;
                    queue.push(new_path);
                }
            }
        }
    }

    println!("{}", counter);
}
