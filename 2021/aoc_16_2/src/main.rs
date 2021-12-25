use std::io::BufRead;

struct Param {
    param_type: ParamType,
    progress: i64,
    nb: i64,
}

enum ParamType {
    Length(i64, i64),
    Number(i64),
}

fn get_int_from_bits(m_iter: &mut std::slice::Iter<bool>, len: i64) -> i64 {
    let mut ret = 0;
    for i in 1..(len + 1) {
        ret += if *m_iter.next().unwrap() { 1 } else { 0 } << len - i;
    }
    ret
}

fn get_packet_info(m_iter: &mut std::slice::Iter<bool>) -> (i64, i64) {
    let ver = get_int_from_bits(m_iter, 3);
    let typ = get_int_from_bits(m_iter, 3);
    (ver, typ)
}

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let message: Vec<bool> = args[0]
        .chars()
        .map(|x| x.to_digit(16).unwrap())
        .map(|x| {
            [
                x & 0b1000 != 0,
                x & 0b0100 != 0,
                x & 0b0010 != 0,
                x & 0b0001 != 0,
            ]
        })
        .flatten()
        .collect();

    let mut m_iter = message.iter();
    let mut val_stack = Vec::new();
    let mut par_stack = Vec::new();
    let mut ope_stack = Vec::new();
    let mut bit_progress = 0;
    while m_iter.clone().count() > 11 {
        let (_, typ) = get_packet_info(&mut m_iter);
        bit_progress += 6;
        if typ != 4 {
            if *m_iter.next().unwrap() {
                // Number
                let number = get_int_from_bits(&mut m_iter, 11);
                bit_progress += 12;
                par_stack.push(Param {
                    param_type: ParamType::Number(number),
                    progress: -1,
                    nb: -1,
                });
            } else {
                // Length
                let length = get_int_from_bits(&mut m_iter, 15);
                bit_progress += 16;
                par_stack.push(Param {
                    param_type: ParamType::Length(bit_progress, length),
                    progress: 0,
                    nb: -1,
                });
            }
            ope_stack.push(typ);
        } else {
            let mut num = 0;
            while *m_iter.next().unwrap() {
                num += get_int_from_bits(&mut m_iter, 4);
                bit_progress += 5;
                num <<= 4;
            }
            num += get_int_from_bits(&mut m_iter, 4);
            bit_progress += 5;
            val_stack.push(num);
        }
        let mut finished = true;
        while par_stack.len() > 0 && finished {
            let val;
            let par_last = par_stack.last_mut().unwrap();
            match par_last.param_type {
                ParamType::Number(n) => {
                    val = n;
                    par_last.progress += 1;
                    par_last.nb += 1;
                }
                ParamType::Length(b, l) => {
                    val = l;
                    par_last.progress = bit_progress - b;
                    par_last.nb += 1;
                }
            }
            if val == par_last.progress {
                let numbers: Vec<i64> = val_stack
                    .drain((val_stack.len() - par_last.nb as usize)..)
                    .collect();
                val_stack.push(match ope_stack.pop().unwrap() {
                    0 => numbers.iter().sum(),
                    1 => numbers.iter().fold(1, |acc, x| x * acc),
                    2 => *numbers.iter().min().unwrap(),
                    3 => *numbers.iter().max().unwrap(),
                    5 => {
                        if numbers[0] > numbers[1] {
                            1
                        } else {
                            0
                        }
                    }
                    6 => {
                        if numbers[0] < numbers[1] {
                            1
                        } else {
                            0
                        }
                    }
                    7 => {
                        if numbers[0] == numbers[1] {
                            1
                        } else {
                            0
                        }
                    }
                    _ => 0,
                });
                par_stack.pop();
            } else {
                finished = false;
            }
        }
    }
    println!("{}", val_stack[0]);
}
