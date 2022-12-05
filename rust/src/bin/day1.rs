
static DATA : &str = include_str!("../../../data/input1.txt");

fn main() {
    let elves = parse_data(DATA);
    let max_cal = calculate(&elves, 1);
    println!("{}", max_cal);
    let top3 = calculate(&elves, 3);
    println!("{}", top3);
}

fn parse_data(input : &str) -> Vec<Vec<i32>> {
    let mut out = Vec::new();
    let elves = input.split("\n\n");
    for elf in elves {
        let cal : Vec<i32> = elf.split('\n').filter(|s| !s.is_empty())
                                .map(|s| s.parse().unwrap()).collect();
        out.push(cal);
    }
    out
}

fn calculate(elves : &Vec<Vec<i32>>, n : usize) -> i32 {
    let mut totals : Vec<i32> = elves.iter().map(|elf| elf.iter().sum()).collect();
    totals.sort_by_key(|n| -n);
    totals.into_iter().take(n).sum()
}

#[cfg(test)]
static TEST_DATA :&str = r#"
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"#;

#[test]
fn test_part1() {
    let elves = parse_data(TEST_DATA);
    let res = calculate(&elves, 1);
    assert_eq!(res, 24000);
}

#[test]
fn test_part2() {
    let elves = parse_data(TEST_DATA);
    let res = calculate(&elves, 3);
    assert_eq!(res, 45000);
}