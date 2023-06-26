use rose_frontend::parse;
use rose_interp::{interp, Val};

#[test]
fn test_add() {
    let src = include_str!("add.rose");
    let module = parse(src).unwrap();
    let answer = interp(
        module.funcs.get("add").unwrap(),
        &[],
        vec![Val::F64(2.), Val::F64(2.)],
    )
    .unwrap();
    assert_eq!(answer, vec![Val::F64(4.)]);
}

#[test]
fn test_sub() {
    let src = include_str!("sub.rose");
    let module = parse(src).unwrap();
    let answer = interp(
        module.funcs.get("sub").unwrap(),
        &[],
        vec![Val::F64(2.), Val::F64(2.)],
    )
    .unwrap();
    assert_eq!(answer, vec![Val::F64(0.)]);
}
