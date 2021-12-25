use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let split_args: Vec<Vec<&str>> = args
        .iter()
        .map(|x| x.split(' ').collect::<Vec<&str>>())
        .collect();

    let sv_args: Vec<(char, i32)> = split_args
        .iter()
        .map(|x| (x[0].chars().next().unwrap(), x[1].parse::<i32>().unwrap()))
        .collect();

    let mut counter_aim = 0;
    let mut counter_horiz = 0;
    let mut counter_depth = 0;

    for (c, i) in sv_args {
        match c {
            'f' => {
                counter_horiz += i;
                counter_depth += counter_aim * i
            }
            'd' => counter_aim += i,
            'u' => counter_aim -= i,
            _ => panic!(),
        }
    }

    println!("{}", counter_depth * counter_horiz);
}
