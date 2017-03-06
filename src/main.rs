#![feature(plugin, custom_attribute)]
#![plugin(stanlib)]
#![allow(dead_code)]

#[condition(pre="true", post="ret == 10:i32")]
fn loopy12() -> i32 {
    let mut a = 0;
    let mut b = 0;

    while a < 4 {
        a += 1;
        b += a;
    }
    b
}

#[condition(pre="x < 100:i32 && x > 10:i32", post="ret == (x * 4:i32)")]
fn loopy1(x: i32) -> i32 {
    let mut a = 0;
    let mut b = 0;

    while a < 4 {
        a += 1;
        b += x;
    }
    b
}

#[condition(pre="x < 100:i32 && x > 10:i32", post="ret == (x / 2:i32)")]
fn divide_two(x:i32) -> i32 {
    x >> 1
}

#[condition(pre="x < 100:i32 && x > 10:i32 && y < 10:i32 && y > 1:i32", post="ret == (x * 4:i32) / y")]
fn multiply_by_4_div_y(x:i32, y:i32) -> i32 {
    (x << 2) / y
}

#[condition(pre="x < 1000:i32 && x > 1:i32", post="ret == (x < 100:i32 && x > 10:i32)")]
fn check_range(x:i32) -> bool {
    x < 100 && x > 10
}

#[condition(pre="true", post="ret == (x < 100:i32 && x > 10:i32)")]
fn check_range2(x:i32) -> bool {
    x < 100 && x > 10
}

#[condition(pre="true", post="ret == (12:i32 - 1:i32)")]
fn single_number() -> i32 {
    11
}

#[condition(pre="x > 10:i32 && x < 20:i32", post="ret == (x + 1:i32)")]
fn param_plus_one(x: i32) -> i32 {
    x + 1
}

#[condition(pre="true", post="ret == ((x * 2:i32) < 20:i32)")]
fn double_less_than_twenty(x: i32) -> bool {
    (x * 2) < 20
}

#[condition(pre="true", post="ret == (x / 2:i32)")]
fn div_two(x:i32) -> i32 {
    x / 2
}

#[condition(pre="true", post="ret == !x")]
fn boolean_not(x:bool) -> bool {
    if x {
        false
    } else {
        true
    }
}

#[condition(pre="true", post="ret == (x > (y / 2:i32))")]
fn bigger_than_input_div_two(x:i32, y:i32) -> bool {
    x > (y / 2)
}

#[condition(pre="x > y", post="ret == true")]
fn bigger_than_input(x:i32, y:i32) -> bool {
    x > y
}

#[condition(pre="true", post="ret == (x < 5:i32)")]
fn check_less_than_five(x:i32) -> bool {
    x < 5
}

#[condition(pre="true", post="ret == (x < 8:i32)")]
fn check_less_than_eight(x: i32) -> bool {
    if x < 8 {
        true
    } else {
        false
    }
}

#[condition(pre="true", post="ret == (x > 0:i32)")]
fn positive(x: i32) -> bool {
    x > 0
}

#[condition(pre="true", post="ret == (x < 0:i32)")]
fn negative(x: i32) -> bool {
    x < 0
}

#[condition(pre="true", post="ret == !x")]
fn boolean_not3(x:bool) -> bool {
    !x
}

#[condition(pre="true", post="(x == true => ret == false) && (x == false => ret == true)")]
fn boolean_not2(x:bool) -> bool {
    if x {
        false
    } else {
        true
    }
}

#[condition(pre="true", post="ret == (x + 1:i32)")]
fn param_plus_one2(x: i32) -> i32 {
    x + 1
}

#[condition(pre="true", post="ret == (x + 5:i32)")]
fn add_five(x:i32) -> i32 {
    x + 5
}

#[condition(pre="true", post="ret == (x - 5:i32)")]
fn param_minus_five(x: i32) -> i32 {
    x - 5
}

fn main() {}