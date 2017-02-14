#![feature(plugin, custom_attribute)]
#![plugin(stanlib)]
#![allow(dead_code)]

#[condition(pre="true", post="ret == (x < 8:i32)")]
fn check_less_than_eight(x: i32) -> bool {
    if x < 8 { //buggy - remove equals
        true
    } else {
        false
    }
}

#[condition(pre="true", post="ret == (x - 5:i32)")]
fn param_minus_five(x: i32) -> i32 {
    x - 5
}

#[condition(pre="true", post="ret == (x < 5:i32)")]
fn check_less_than_five(x:i32) -> bool {
    x < 5 // buggy - change to 5
}

#[condition(pre="true", post="ret == !x")]
fn boolean_not(x:bool) -> bool {
    if x == true {
        false // buggy - change to false
    } else {
        true
    }
}

#[condition(pre="true", post="(x == true => ret == false) && (x == false => ret == true)")]
fn boolean_not2(x:bool) -> bool {
    if x == true {
        false
    } else {
        true
    }
}

#[condition(pre="x <= 100:i32 - 5:i32", post="ret == (x + 5:i32)")]
fn add_five(x:i32) -> i32 {
    assert!(x <= 100 - 5);
    x + 5
}

/*
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
fn main() {
}