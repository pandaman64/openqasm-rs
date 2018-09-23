extern crate openqasm;
use openqasm::from_str;

#[test]
fn test_qft3() {
    let input = include_str!("qe_qft_3.qasm");
    from_str(input).unwrap();
}
