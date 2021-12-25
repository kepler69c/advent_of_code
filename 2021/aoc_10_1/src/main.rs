use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let mut counter = 0;
    for line in args {
        let mut stack = vec!['X'];
        for c in line.chars() {
            match c {
                '(' | '[' | '{' | '<' => stack.push(c),
                ')' => {
                    if stack.pop().unwrap() != '(' {
                        counter += 3;
                        break;
                    }
                }

                ']' => {
                    if stack.pop().unwrap() != '[' {
                        counter += 57;
                        break;
                    }
                }

                '}' => {
                    if stack.pop().unwrap() != '{' {
                        counter += 1197;
                        break;
                    }
                }

                '>' => {
                    if stack.pop().unwrap() != '<' {
                        counter += 25137;
                        break;
                    }
                }

                _ => (),
            }
        }
    }

    println!("{}", counter);
}
