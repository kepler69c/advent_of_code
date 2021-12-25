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

    let mut tx = rx.0;
    let mut x = 0;
    while tx > 0 {
        x += 1;
        tx -= x;
    }

    let mut y = 89;
    let mut ny = 90;
    let mut max_y = 0;
    let mut hit_target = true;
    while hit_target {
        let mut ty = 0;
        let mut fy = ny;
        let mut new_max_y = max_y;
        println!("init: {:?}", ny);
        while ty > ry.1 {
            ty += fy;
            fy -= 1;
            println!("launched: {:?}", ty);
            if new_max_y < ty {
                new_max_y = ty;
            }
        }
        if ty < ry.0 {
            hit_target = false;
        } else {
            max_y = new_max_y;
            y = ny;
            ny += 1;
        }
    }

    println!("{:?}", (x, y, max_y));
}
