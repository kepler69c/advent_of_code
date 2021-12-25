use std::collections::HashMap;
use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let signal_digits: Vec<(Vec<&str>, Vec<&str>)> = args
        .iter()
        .map(|x| {
            let mut split = x.split('|');
            (
                split.next().unwrap().split_whitespace().collect(),
                split.next().unwrap().split_whitespace().collect(),
            )
        })
        .collect();

    let seven_seg_digits = [
        [true, true, true, false, true, true, true],     // 0
        [false, false, true, false, false, true, false], // 1
        [true, false, true, true, true, false, true],    // 2
        [true, false, true, true, false, true, true],    // 3
        [false, true, true, true, false, true, false],   // 4
        [true, true, false, true, false, true, true],    // 5
        [true, true, false, true, true, true, true],     // 6
        [true, false, true, false, false, true, false],  // 7
        [true, true, true, true, true, true, true],      // 8
        [true, true, true, true, false, true, true],     // 9
    ];

    let mut numbers = 0;
    for (signal, digits) in signal_digits {
        let one: &str = signal.iter().find(|x| x.len() == 2).unwrap();
        let four: &str = signal.iter().find(|x| x.len() == 4).unwrap();
        let seven: &str = signal.iter().find(|x| x.len() == 3).unwrap();

        let mut two_three_five: [&str; 3] = [""; 3];
        let mut cons_signal = signal.iter();
        two_three_five[0] = cons_signal.find(|x| x.len() == 5).unwrap();
        two_three_five[1] = cons_signal.find(|x| x.len() == 5).unwrap();
        two_three_five[2] = cons_signal.find(|x| x.len() == 5).unwrap();

        const VEC: Vec<char> = Vec::new();
        let mut diods = [VEC; 7];

        // add 1
        for c in one.chars() {
            diods[2].push(c);
            diods[5].push(c);
        }

        // add 7
        for c in seven.chars() {
            if !diods[2].contains(&c) && !diods[5].contains(&c) {
                diods[0].push(c);
            }
        }

        // add 4
        for c in four.chars() {
            if !diods[2].contains(&c) && !diods[5].contains(&c) {
                diods[1].push(c);
                diods[3].push(c);
            }
        }

        // filter with two_three_five
        let mut count_two_three_five: HashMap<char, u8> = HashMap::new();
        for c in two_three_five.iter().map(|x| x.chars()).flatten() {
            let count = count_two_three_five.entry(c).or_insert(0);
            *count += 1;
            if *count == 3 && diods[3].contains(&c) {
                diods[3].retain(|&x| x == c);
                diods[1].retain(|&x| x != c);
                break;
            }
        }

        // find 5 and complete
        for num in two_three_five {
            for c in num.chars() {
                if diods[1].contains(&c) {
                    for c in num.chars() {
                        if diods[5].contains(&c) {
                            diods[5].retain(|&x| x == c);
                            diods[2].retain(|&x| x != c);
                        }
                        if diods.iter().flatten().find(|&&x| x == c).is_none() {
                            diods[6].push(c);
                        }
                    }
                    break;
                }
            }
        }

        // deduce final diode
        diods[4].push(
            "abcdefg"
                .chars()
                .find(|&x| diods.iter().flatten().find(|&&y| y == x).is_none())
                .unwrap(),
        );

        let final_diods: String = diods.iter().flat_map(|x| x).collect();

        for (i, s_digit) in digits.iter().enumerate() {
            let toggled_diods: Vec<bool> =
                final_diods.chars().map(|x| s_digit.contains(x)).collect();
            let digit = seven_seg_digits
                .iter()
                .enumerate()
                .map(|(v, x)| {
                    if x.iter().eq(toggled_diods.iter()) {
                        Some(v)
                    } else {
                        None
                    }
                })
                .flatten()
                .next()
                .unwrap();
            numbers += 10usize.pow((digits.len() - i - 1) as u32) * digit;
        }
    }

    println!("{}", numbers);
}
