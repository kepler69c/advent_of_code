use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let coords: Vec<((i32, i32), (i32, i32))> = args
        .iter()
        .map(|x| {
            let parse = x
                .split(" -> ")
                .map(|y| {
                    y.split(',')
                        .map(|z| z.parse::<i32>().unwrap())
                        .collect::<Vec<i32>>()
                })
                .collect::<Vec<Vec<i32>>>();
            ((parse[0][0], parse[0][1]), (parse[1][0], parse[1][1]))
        })
        .collect();

    let (mut xmin, mut xmax, mut ymin, mut ymax) = (i32::MAX, i32::MIN, i32::MAX, i32::MIN);
    for ((x1, y1), (x2, y2)) in coords.iter() {
        xmin = if *x1 < xmin { *x1 } else { xmin };
        xmin = if *x2 < xmin { *x2 } else { xmin };
        xmax = if *x1 > xmax { *x1 } else { xmax };
        xmax = if *x2 > xmax { *x2 } else { xmax };
        ymin = if *y1 < ymin { *y1 } else { ymin };
        ymin = if *y2 < ymin { *y2 } else { ymin };
        ymax = if *y1 > ymax { *y1 } else { ymax };
        ymax = if *y2 > ymax { *y2 } else { ymax };
    }

    let mut map = vec![vec![0; (ymax - ymin + 1) as usize]; (xmax - xmin + 1) as usize];

    for ((x1, y1), (x2, y2)) in coords.iter() {
        if *x1 == *x2 {
            let s = std::cmp::min(*y1, *y2);
            let e = std::cmp::max(*y1, *y2);
            for i in s..(e + 1) {
                map[(*x1 - xmin) as usize][(i - ymin) as usize] += 1;
            }
        } else if *y1 == *y2 {
            let s = std::cmp::min(*x1, *x2);
            let e = std::cmp::max(*x1, *x2);
            for i in s..(e + 1) {
                map[(i - xmin) as usize][(*y1 - ymin) as usize] += 1;
            }
        }
    }

    let mut cnt_max = 0;
    for line in map.iter() {
        for val in line.iter() {
            if *val >= 2 {
                cnt_max += 1;
            }
        }
    }

    println!("{}", cnt_max);
}
