# stanley

[![Build Status](https://travis-ci.org/neosilky/stanley.svg?branch=master)](https://travis-ci.org/neosilky/stanley)

**VERY ALPHA**

Stanley is a Rust compiler plugin to verify the semantics of functions via given {pre,post}conditions. It is based upon [RustProof](https://github.com/Rust-Proof/rustproof/) but is generally a reimplementation and feature improvement.

This project is dual licensed under MIT and Apache-2.0.

BiExp(
	Bool(true)
		=>
	BiExp(
		BiExp(
			VarMap("tmp4":i32)
				=>
			BiExp(
				VarMap("ret":i32)
					==
				BiExp(
					VarMap("x":i32)
						-
					BitVec(5:i32)
				)
			)
		)
		&&
		BiExp(
			UnExp(
				NOT
				VarMap("tmp4":i32)
			)
				=>
			BiExp(
				VarMap("ret":i32)
					==
				BiExp(
					VarMap("x":i32)
						-
					BitVec(5:i32))
				)
			)
		)
	)
