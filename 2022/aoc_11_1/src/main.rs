use std::io::BufRead;

#[derive(Copy, Clone)]
enum Num {
    Old,
    N(usize),
}

#[derive(Copy, Clone)]
enum Op {
    Plus(Num),
    Mult(Num),
}

fn main() {
    let args: Vec<_> = std::io::stdin().lock().lines().flatten().collect();

    let mut monkeys: Vec<_> = args
        .chunks(7)
        .into_iter()
        .map(|x| {
            let items: Vec<_> = x[1][18..]
                .split_whitespace()
                .map(|mut x| {
                    if let Some(y) = x.strip_suffix(',') {
                        x = y;
                    }
                    x.parse::<usize>().unwrap()
                })
                .collect();
            let op_n = match &x[2][25..] {
                "old" => Num::Old,
                n => Num::N(n.parse::<usize>().unwrap()),
            };
            let op = if &x[2][23..24] == "+" {
                Op::Plus(op_n)
            } else {
                Op::Mult(op_n)
            };
            let test = (
                x[3][21..].parse::<usize>().unwrap(),
                x[4][29..].parse::<usize>().unwrap(),
                x[5][30..].parse::<usize>().unwrap(),
            );
            (items, op, test)
        })
        .collect();

    let mut insp = (0..20).fold(vec![0; monkeys.len()], |acc, _| {
        (0..monkeys.len())
            .map(|i| {
                let (items, op, test) = monkeys[i].clone();
                let n = items.len();
                items.iter().for_each(|item| {
                    let worry = match op {
                        Op::Plus(v) => {
                            item + match v {
                                Num::Old => *item,
                                Num::N(v) => v,
                            }
                        }
                        Op::Mult(v) => {
                            item * match v {
                                Num::Old => *item,
                                Num::N(v) => v,
                            }
                        }
                    } / 3;
                    monkeys[if worry % test.0 == 0 { test.1 } else { test.2 }]
                        .0
                        .push(worry);
                });
                monkeys[i].0.clear();
                n
            })
            .zip(acc.iter())
            .map(|(a, b)| a + b)
            .collect()
    });

    insp.sort();

    println!("{}", insp.into_iter().rev().take(2).product::<usize>());
}
