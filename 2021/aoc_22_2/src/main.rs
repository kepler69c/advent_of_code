use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let on_off: Vec<bool> = args
        .iter()
        .map(|x| x.split(' ').next().unwrap() == "on")
        .collect();

    let cubes: Vec<((i32, i32), (i32, i32), (i32, i32))> = args
        .iter()
        .map(|k| {
            let k = k.split(' ').skip(1).next().unwrap();
            let mut k = k.split(',');
            let x = k.next().unwrap();
            let y = k.next().unwrap();
            let z = k.next().unwrap();
            let x = x.split('=').skip(1).next().unwrap();
            let mut x = x.split("..");
            let x = (
                x.next().unwrap().parse().unwrap(),
                x.next().unwrap().parse().unwrap(),
            );
            let y = y.split('=').skip(1).next().unwrap();
            let mut y = y.split("..");
            let y = (
                y.next().unwrap().parse().unwrap(),
                y.next().unwrap().parse().unwrap(),
            );
            let z = z.split('=').skip(1).next().unwrap();
            let mut z = z.split("..");
            let z = (
                z.next().unwrap().parse().unwrap(),
                z.next().unwrap().parse().unwrap(),
            );
            (x, y, z)
        })
        .collect();
}
