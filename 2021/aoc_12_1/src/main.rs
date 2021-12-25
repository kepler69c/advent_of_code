use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::io::BufRead;

#[derive(Debug)]
struct Node<'a> {
    name: &'a str,
    small: bool,
}

impl Node<'_> {
    fn start() -> Self {
        Node {
            name: "start",
            small: true,
        }
    }
    fn end() -> Self {
        Node {
            name: "end",
            small: true,
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
                },
                Node {
                    name: t.1,
                    small: t.1.chars().next().unwrap().is_lowercase(),
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
    let mut queue: Vec<Vec<&Node>> = Vec::new();
    queue.push(vec![&root]);
    while queue.len() > 0 {
        let path = queue.pop().unwrap();
        let last = *path.last().unwrap();
        if last == &Node::end() {
            counter += 1;
        }
        if let Some(list) = graph.get(last) {
            for n in list {
                if !n.small || !path.contains(n) {
                    let mut new_path = path.clone();
                    new_path.push(n);
                    queue.push(new_path);
                }
            }
        }
    }

    println!("{}", counter);
}
