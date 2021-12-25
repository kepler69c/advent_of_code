use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();
    let mut contests = Vec::new();

    for line in args.iter() {
        let mut stack = vec!['X'];
        let mut corrupted = false;
        for c in line.chars() {
            match c {
                '(' | '[' | '{' | '<' => stack.push(c),
                ')' => {
                    if stack.pop().unwrap() != '(' {
                        corrupted = true;
                        break;
                    }
                }
                ']' => {
                    if stack.pop().unwrap() != '[' {
                        corrupted = true;
                        break;
                    }
                }
                '}' => {
                    if stack.pop().unwrap() != '{' {
                        corrupted = true;
                        break;
                    }
                }
                '>' => {
                    if stack.pop().unwrap() != '<' {
                        corrupted = true;
                        break;
                    }
                }
                _ => (),
            }
        }
        if !corrupted {
            contests.push(stack.iter().skip(1).rev().fold(0 as u64, |acc, x| {
                acc * 5
                    + match x {
                        '(' => 1,
                        '[' => 2,
                        '{' => 3,
                        '<' => 4,
                        _ => 0,
                    }
            }));
        }
    }

    contests.sort();
    println!("{}", contests[contests.len() / 2]);
}
