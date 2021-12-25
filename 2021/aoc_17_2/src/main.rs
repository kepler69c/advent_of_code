use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let (rx, ry) = {
        let mut ranges = args[0].split(':').nth(1).unwrap().split(',').map(|x| {
            let mut range = x
                .split('=')
                .nth(1)
                .unwrap()
                .split("..")
                .map(|x| x.parse::<i64>().unwrap());
            (range.next().unwrap(), range.next().unwrap())
        });
        (ranges.next().unwrap(), ranges.next().unwrap())
    };

    let mut configs = Vec::new();
    for x in 0..rx.1 + 1 {
        for y in ry.0..-ry.0 + 1 {
            let (mut tx, mut ty) = (0, 0);
            let (mut fx, mut fy) = (x, y);
            while tx <= rx.1 && ty >= ry.0 {
                tx += fx;
                ty += fy;
                if fx > 0 {
                    fx -= 1;
                }
                fy -= 1;
                if ty <= ry.1 && ty >= ry.0 && tx <= rx.1 && tx >= rx.0 {
                    configs.push((x, y));
                    break;
                }
            }
        }
    }

    println!("{:?}, {}", configs, configs.len());
}
