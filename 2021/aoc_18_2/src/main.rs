use std::cell::RefCell;
use std::io::BufRead;
use std::rc::Rc;

#[derive(Debug)]
enum Number {
    Pair(Rc<RefCell<Number>>, Rc<RefCell<Number>>),
    Value(u32),
}

fn build_tree(tree: &Vec<Option<u32>>, index: usize) -> Number {
    if let Some(v) = tree[index] {
        Number::Value(v)
    } else {
        Number::Pair(
            Rc::new(RefCell::new(build_tree(tree, 2 * (index + 1) - 1))),
            Rc::new(RefCell::new(build_tree(tree, 2 * (index + 1)))),
        )
    }
}

fn get_tree(string: &String) -> Number {
    let mut depth = 0;
    let mut tree = vec![None];
    {
        let mut index = 1;
        string.chars().fold(0, |acc, x| {
            if depth < acc {
                depth = acc;
                tree.append(&mut vec![None; 1 << depth]);
            };
            match x {
                '[' => {
                    index <<= 1;
                    acc + 1
                }
                ']' => {
                    index >>= 1;
                    acc - 1
                }
                ',' => {
                    index += 1;
                    acc
                }
                '0'..='9' => {
                    tree[index - 1] = Some(x.to_digit(10).unwrap());
                    acc
                }
                _ => acc,
            }
        });
    }

    build_tree(&tree, 0)
}

fn reduce_number(number: Rc<RefCell<Number>>) {
    let mut values = Vec::new();
    let mut removal = Vec::new();
    let mut queue = vec![(number.clone(), 0)];
    while queue.len() > 0 {
        let (u, d) = queue.pop().unwrap();
        match &*u.clone().borrow() {
            Number::Value(_) => values.push((u.clone(), d)),
            Number::Pair(n1, n2) => {
                if d == 4 {
                    removal.push(u.clone());
                }
                queue.push((n2.clone(), d + 1));
                queue.push((n1.clone(), d + 1));
            }
        }
    }
    loop {
        if let Some((i, _)) = values.iter().enumerate().find(|x| x.1 .1 > 4) {
            if values[i].1 > 4 {
                if i > 0 {
                    let val_l = if let Number::Value(v) = *values[i].0.borrow() {
                        v
                    } else {
                        panic!();
                    };
                    let val_lm = if let Number::Value(v) = *values[i - 1].0.borrow() {
                        v
                    } else {
                        panic!();
                    };
                    values[i - 1].0.replace(Number::Value(val_l + val_lm));
                }
                if i + 1 < values.len() - 1 {
                    let val_r = if let Number::Value(v) = *values[i + 1].0.borrow() {
                        v
                    } else {
                        panic!();
                    };
                    let val_rm = if let Number::Value(v) = *values[i + 2].0.borrow() {
                        v
                    } else {
                        panic!();
                    };
                    values[i + 2].0.replace(Number::Value(val_r + val_rm));
                }
                removal[0].replace(Number::Value(0));
                values.remove(i);
                values.remove(i);
                values.insert(i, (removal[0].clone(), 4));
                removal.remove(0);
                continue;
            }
        }
        if let Some((i, _)) = values.iter().enumerate().find(|x| {
            if let Number::Value(v) = *x.1 .0.borrow() {
                v > 9
            } else {
                panic!();
            }
        }) {
            let val = if let Number::Value(v) = *values[i].0.borrow() {
                v
            } else {
                panic!();
            };
            let left = Rc::new(RefCell::new(Number::Value(val >> 1)));
            let right = Rc::new(RefCell::new(Number::Value((val >> 1) + (val & 1))));
            let d = values[i].1;
            values[i]
                .0
                .replace(Number::Pair(left.clone(), right.clone()));
            if d + 1 > 4 {
                removal.push(values[i].0.clone());
            }
            values.remove(i);
            values.insert(i, (right, d + 1));
            values.insert(i, (left, d + 1));
            continue;
        }
        break;
    }
}

fn magnitude(number: Rc<RefCell<Number>>) -> u32 {
    match &*number.clone().borrow() {
        Number::Pair(n1, n2) => 3 * magnitude(n1.clone()) + 2 * magnitude(n2.clone()),
        Number::Value(v) => *v,
    }
}

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let mut max = 0;
    let trees: Vec<Rc<RefCell<Number>>> = args
        .iter()
        .map(|x| Rc::new(RefCell::new(get_tree(x))))
        .collect();

    for i in 0..trees.len() {
        for j in 0..trees.len() {
            if i != j {
                let t1 = Rc::new(RefCell::new(Number::Pair(
                    Rc::new(RefCell::new(get_tree(&args[i]))),
                    Rc::new(RefCell::new(get_tree(&args[j]))),
                )));
                let t2 = Rc::new(RefCell::new(Number::Pair(
                    Rc::new(RefCell::new(get_tree(&args[j]))),
                    Rc::new(RefCell::new(get_tree(&args[i]))),
                )));
                reduce_number(t1.clone());
                reduce_number(t2.clone());
                max = *[max, magnitude(t1), magnitude(t2)].iter().max().unwrap();
            }
        }
    }

    println!("{:?}", max);
}
