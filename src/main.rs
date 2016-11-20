#![feature(plugin, custom_attribute)]
#![plugin(stanlib)]

#![allow(dead_code)]

fn main() {
}

#[condition(pre="true", post="ret == (x - 5:i32)")]
fn param_minus_five(x: i32) -> i32 {
    x - 5
}

#[condition(pre="true", post="ret == (x < 10:i32)")]
fn check_less_than_ten(x: i32) -> bool {
    if x <= 10 { //buggy
        true
    } else {
        false
    }
}

#[condition(pre="true", post="ret == (x < 5:i32)")]
fn check_less_than_five(x:i32) -> bool {
    x < 5
}

#[condition(pre="true", post="ret == !x")]
fn boolean_not(x:bool) -> bool {
    if x == true {
        false
    } else {
        true
    }
}

#[condition(pre="true", post="(x == true IMPLIES ret == false) && (x == false IMPLIES ret == true)")]
fn boolean_not2(x:bool) -> bool {
    if x == true {
        false
    } else {
        true
    }
}

/*
#[condition(pre="(x <= i32::MAX - 5:i32)", post="return == (x + 5:i32)")]
fn add_five(x:i32) -> i32 {
    assert!(x <= 2147483647-5);
    x+5
}

#[condition(pre="true", post="ret sorted(xs)")]
fn selection(xs: &mut [u32]) {
    let (mut i, len) = (0, xs.len());
    while i < len {
        let (mut j, mut cur_min) = (i + 1, i);
        while j < len {
            if xs[j] < xs[cur_min] {
                cur_min = j;
            }
            j = j + 1;
        }
        xs.swap(i, cur_min);
        i = i + 1;
    }
}
*/