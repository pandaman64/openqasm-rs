#[macro_use]
extern crate nom;

use std::str::FromStr;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct QuantumRegister {
    name: String,
    size: usize,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ClassicalRegister {
    name: String,
    size: usize,
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Qubit(String, usize);

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Bit(String, usize);

#[derive(Clone, PartialEq, Debug)]
pub enum Operation {
    Unitary(String, Vec<f64>, Qubit),
    Cx(Qubit, Qubit),
    Measure(Qubit, Bit),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Qasm {
    quantum_registers: Vec<QuantumRegister>,
    classical_registers: Vec<ClassicalRegister>,
    operations: Vec<Operation>,
}

#[derive(Clone, PartialEq, Debug)]
enum Line {
    Qreg(QuantumRegister),
    Creg(ClassicalRegister),
    Op(Operation),
}

pub fn from_str(input: &str) -> nom::IResult<&str, Qasm> {
    program(input)
}

fn is_whitespace(c: char) -> bool {
    c == '\t' || c == ' '
}

named!(program<&str, Qasm>,
    do_parse!(
        lines: separated_list!(
            tag!("\n"),
            line
        )
        >> (lines.into_iter()
            .flat_map(|x| x)
            .fold(Qasm {
                quantum_registers: vec![],
                classical_registers: vec![],
                operations: vec![],
            }, |mut qasm, line| {
                match line {
                    Line::Qreg(qreg) => qasm.quantum_registers.push(qreg),
                    Line::Creg(creg) => qasm.classical_registers.push(creg),
                    Line::Op(op) => qasm.operations.push(op),
                }
                qasm
            }))
    )
);
named!(line<&str, Option<Line>>,
    delimited!(
        opt!(complete!(call!(sp))),
        alt_complete!(
            value!(None, comment)
            | map!(qreg, |x| Some(Line::Qreg(x)))
            | map!(creg, |x| Some(Line::Creg(x)))
            | do_parse!(
                op: operation
                >> call!(sp)
                >> opt!(complete!(comment))
                >> (Some(Line::Op(op)))
            )
            | map!(operation, |x| Some(Line::Op(x)))
            | value!(None, nom::rest_s) // ignoring any invalid lines
        ),
        opt!(complete!(call!(sp)))
    )
);
named!(operation<&str, Operation>,
    alt_complete!(
        cx
        | measure
        | unitary
    )
);
named!(unitary<&str, Operation>,
    do_parse!(
        name: call!(nom::alpha)
        >> call!(sp)
        >> params: opt!(complete!(delimited!(
            terminated!(tag!("("), call!(sp)),
            separated_list!(
                terminated!(tag!(","), call!(sp)),
                terminated!(call!(nom::double_s), call!(sp))
            ),
            tag!(")")
        )))
        >> call!(sp)
        >> q: qubit
        >> (Operation::Unitary(name.to_string(), params.unwrap_or(vec![]), q))
    )
);
named!(cx<&str, Operation>,
    do_parse!(
        tag!("CX")
        >> call!(sp)
        >> q1: qubit
        >> call!(sp)
        >> tag!(",")
        >> call!(sp)
        >> q2: qubit
        >> (Operation::Cx(q1, q2))
    )
);
named!(measure<&str, Operation>,
    do_parse!(
        tag!("measure")
        >> call!(sp)
        >> q: qubit
        >> call!(sp)
        >> tag!("->")
        >> call!(sp)
        >> c: bit
        >> (Operation::Measure(q, c))
    )
);

named_args!(register<'a>(t: &'a str)<&'a str, (String, usize)>,
    do_parse!(
        tag!(t)
        >> call!(sp)
        >> name: call!(nom::alpha)
        >> call!(sp)
        >> tag!(",")
        >> call!(sp)
        >> size: map_res!(nom::digit, FromStr::from_str)
        >> ((name.to_string(), size))
    )
);
named!(qreg<&str, QuantumRegister>, map!(call!(register, "qreg"), |(name, size)| QuantumRegister {
    name,
    size,
}));
named!(creg<&str, ClassicalRegister>, map!(call!(register, "creg"), |(name, size)| ClassicalRegister {
    name,
    size,
}));

named!(operand<&str, (String, usize)>,
    do_parse!(
        name: call!(nom::alpha)
        >> call!(sp)
        >> tag!("[")
        >> call!(sp)
        >> index: map_res!(nom::digit, FromStr::from_str)
        >> call!(sp)
        >> tag!("]")
        >> (name.to_string(), index)
    )
);
named!(qubit<&str, Qubit>, map!(operand, |t| Qubit(t.0, t.1)));
named!(bit<&str, Bit>, map!(operand, |t| Bit(t.0, t.1)));

named!(comment<&str, &str>,
    recognize!(preceded!(
        tag!("//"),
        nom::rest_s
    ))
);

named!(sp<&str, &str>, take_while!(is_whitespace));

#[test]
fn test_program() {
    assert_eq!(
        program(
            r#"qreg q  , 5
creg c  , 4
U(1.2, 3, 4.56) q[3]
U(7, 8, 9) quuu[2]
CX qu[2], q[3]
X q [ 6]
measure q [ 1 ] -> c [ 3 ]
"#
        ),
        Ok((
            "\n",
            Qasm {
                quantum_registers: vec![QuantumRegister {
                    name: "q".to_string(),
                    size: 5
                },],
                classical_registers: vec![ClassicalRegister {
                    name: "c".to_string(),
                    size: 4,
                },],
                operations: vec![
                    Operation::Unitary(
                        "U".to_string(),
                        vec![1.2, 3.0, 4.56],
                        Qubit("q".to_string(), 3)
                    ),
                    Operation::Unitary(
                        "U".to_string(),
                        vec![7.0, 8.0, 9.0],
                        Qubit("quuu".to_string(), 2)
                    ),
                    Operation::Cx(Qubit("qu".to_string(), 2), Qubit("q".to_string(), 3)),
                    Operation::Unitary("X".to_string(), vec![], Qubit("q".to_string(), 6)),
                    Operation::Measure(Qubit("q".to_string(), 1), Bit("c".to_string(), 3)),
                ],
            }
        ))
    );
}

#[test]
fn test_operation() {
    assert_eq!(
        operation("U(1.2, 3, 4.56)q[3]"),
        Ok((
            "",
            Operation::Unitary(
                "U".to_string(),
                vec![1.2, 3.0, 4.56],
                Qubit("q".to_string(), 3)
            )
        ))
    );
}

#[test]
fn test_line() {
    assert_eq!(line(""), Ok(("", None)));
    assert_eq!(line("// hoge"), Ok(("", None)));
    assert_eq!(
        line("  U(0,1,-1)q[0]"),
        Ok((
            "",
            Some(Line::Op(Operation::Unitary(
                "U".to_string(),
                vec![0.0, 1.0, -1.0,],
                Qubit("q".to_string(), 0)
            )))
        ))
    );
    assert_eq!(
        line("\tU(0,1,-1)q[100]// hoge"),
        Ok((
            "",
            Some(Line::Op(Operation::Unitary(
                "U".to_string(),
                vec![0.0, 1.0, -1.0,],
                Qubit("q".to_string(), 100)
            )))
        ))
    );
    assert_eq!(
        line(" U(0,1,-1)q[100] // hoge"),
        Ok((
            "",
            Some(Line::Op(Operation::Unitary(
                "U".to_string(),
                vec![0.0, 1.0, -1.0,],
                Qubit("q".to_string(), 100)
            )))
        ))
    );
}

#[test]
fn test_comment() {
    assert_eq!(comment("// hoge"), Ok(("", "// hoge")));
    assert!(comment("hoge").is_err());
}
