#[macro_use]
extern crate nom;
#[macro_use]
extern crate log;

use std::str::FromStr;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct QuantumRegister {
    pub name: String,
    pub size: usize,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ClassicalRegister {
    pub name: String,
    pub size: usize,
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Qubit(pub String, pub usize);

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Bit(pub String, pub usize);

#[derive(Clone, PartialEq, Debug)]
pub enum Operation {
    Unitary(String, Vec<f64>, Qubit),
    Cx(Qubit, Qubit),
    Measure(Qubit, Bit),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Qasm {
    pub quantum_registers: Vec<QuantumRegister>,
    pub classical_registers: Vec<ClassicalRegister>,
    pub operations: Vec<Operation>,
}

#[derive(Clone, PartialEq, Debug)]
enum Statement {
    Qreg(QuantumRegister),
    Creg(ClassicalRegister),
    Op(Operation),
    Include(String),
}

pub fn from_str<'a>(input: &str) -> nom::IResult<&str, Qasm> {
    program(input)
}

macro_rules! w (
    ($i:expr, $($args:tt)*) => (
        {
            sep!($i, separator, $($args)*)
        }
    )
);

named!(program<&str, Qasm>,
    w!(do_parse!(
        tag!("OPENQASM")
        >> tag!("2.0")
        >> tag!(";")
        >> statements: many1!(statement)
        >> (statements.into_iter()
            .fold(Qasm {
                quantum_registers: vec![],
                classical_registers: vec![],
                operations: vec![],
            }, |mut qasm, statement| {
                match statement {
                    Statement::Qreg(qreg) => qasm.quantum_registers.push(qreg),
                    Statement::Creg(creg) => qasm.classical_registers.push(creg),
                    Statement::Op(op) => qasm.operations.push(op),
                    Statement::Include(_) => debug!("currently ignoring include"),
                }
                qasm
            }))
    ))
);
named!(statement<&str, Statement>,
    w!(terminated!(
        alt_complete!(
            map!(include, Statement::Include)
            | map!(qreg, Statement::Qreg)
            | map!(creg, Statement::Creg)
            | map!(operation, Statement::Op)
        ),
        tag!(";")
    ))
);

named!(operation<&str, Operation>,
    w!(alt_complete!(
        cx
        | measure
        | unitary
    ))
);
named!(unitary<&str, Operation>,
    w!(do_parse!(
        name: ident
        >> params: opt!(complete!(w!(delimited!(
            tag!("("),
            separated_list!(
                tag!(","),
                call!(nom::double_s)
            ),
            tag!(")")
        ))))
        >> q: qubit
        >> (Operation::Unitary(name.to_string(), params.unwrap_or(vec![]), q))
    ))
);
named!(cx<&str, Operation>,
    w!(do_parse!(
        tag!("CX")
        >> q1: qubit
        >> tag!(",")
        >> q2: qubit
        >> (Operation::Cx(q1, q2))
    ))
);
named!(measure<&str, Operation>,
    w!(do_parse!(
        tag!("measure")
        >> q: qubit
        >> tag!("->")
        >> c: bit
        >> (Operation::Measure(q, c))
    ))
);

named_args!(register<'a>(t: &'a str)<&'a str, (String, usize)>,
    w!(do_parse!(
        tag!(t)
        >> name: ident
        >> tag!("[")
        >> size: map_res!(nom::digit, FromStr::from_str)
        >> tag!("]")
        >> ((name.to_string(), size))
    ))
);
named!(qreg<&str, QuantumRegister>, map!(call!(register, "qreg"), |(name, size)| QuantumRegister {
    name,
    size,
}));
named!(creg<&str, ClassicalRegister>, map!(call!(register, "creg"), |(name, size)| ClassicalRegister {
    name,
    size,
}));

named!(include<&str, String>,
    w!(do_parse!(
        tag!("include")
        >> filename: delimited!(tag!("\""), take_until!("\""), tag!("\""))
        >> (filename.to_string())
    ))
);

named!(operand<&str, (String, usize)>,
    w!(do_parse!(
        name: ident
        >> tag!("[")
        >> index: map_res!(nom::digit, FromStr::from_str)
        >> tag!("]")
        >> (name.to_string(), index)
    ))
);
named!(qubit<&str, Qubit>, map!(operand, |t| Qubit(t.0, t.1)));
named!(bit<&str, Bit>, map!(operand, |t| Bit(t.0, t.1)));

named!(comment<&str, &str>,
    recognize!(preceded!(
        tag!("//"),
        terminated!(
            take_until!("\n"),
            alt_complete!(eof!() | call!(nom::eol))
        )
    ))
);

named!(ident<&str, &str>, recognize!(pair!(nom::alpha, nom::alphanumeric0)));
named!(separator<&str, &str>, alt_complete!(comment | eat_separator!(" \t\r\n")));

#[test]
fn test_program() {
    assert_eq!(
        program(
            r#"
OPENQASM 2.0;
include "qelib1.inc";

qreg q[5] ;
creg c [4 ]; 
U(1.2, 3, 4.56) q[3]; 
u1(3) qqq[3] ;
U(7, 8, 9) quuu[2] ;
CX qu[2], q[3];
X q [ 6];
measure q [ 1 ] -> c [ 3 ];
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
                    Operation::Unitary("u1".to_string(), vec![3.0], Qubit("qqq".to_string(), 3)),
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
fn test_statement() {
    assert_eq!(
        statement("  U(0,1,-1)q[0];"),
        Ok((
            "",
            Statement::Op(Operation::Unitary(
                "U".to_string(),
                vec![0.0, 1.0, -1.0,],
                Qubit("q".to_string(), 0)
            ))
        ))
    );
    assert_eq!(
        statement("\tU(0,1,-1)q[100];// hoge\n"),
        Ok((
            "// hoge\n",
            Statement::Op(Operation::Unitary(
                "U".to_string(),
                vec![0.0, 1.0, -1.0,],
                Qubit("q".to_string(), 100)
            ))
        ))
    );
    assert_eq!(
        statement("\nU(0,1,-1)q[100]  ; // hoge\n"),
        Ok((
            " // hoge\n",
            Statement::Op(Operation::Unitary(
                "U".to_string(),
                vec![0.0, 1.0, -1.0,],
                Qubit("q".to_string(), 100)
            ))
        ))
    );
}

#[test]
fn test_comment() {
    assert_eq!(comment("// hoge\n"), Ok(("", "// hoge\n")));
    assert!(comment("hoge").is_err());
}
