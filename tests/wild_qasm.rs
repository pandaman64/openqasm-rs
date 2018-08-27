extern crate openqasm_rs;

#[test]
fn test_qft3() {
    let input = include_str!("qe_qft_3.qasm");
    openqasm_rs::from_str(input).unwrap();
}

