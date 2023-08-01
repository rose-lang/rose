use indexmap::IndexSet;
use rose_frontend::parse;
use rose_interp::{interp, val_f64};

#[test]
fn test_add() {
    let src = include_str!("add.rose");
    let module = parse(src).unwrap();
    let answer = interp(
        module.get_func("add").unwrap(),
        IndexSet::new(),
        &[],
        [val_f64(2.), val_f64(2.)].into_iter(),
    )
    .unwrap();
    assert_eq!(answer, val_f64(4.));
}

#[test]
fn test_sub() {
    let src = include_str!("sub.rose");
    let module = parse(src).unwrap();
    let answer = interp(
        module.get_func("sub").unwrap(),
        IndexSet::new(),
        &[],
        [val_f64(2.), val_f64(2.)].into_iter(),
    )
    .unwrap();
    assert_eq!(answer, val_f64(0.));
}

#[test]
fn test_mul() {
    let src = include_str!("mul.rose");
    let module = parse(src).unwrap();
    let answer = interp(
        module.get_func("mul").unwrap(),
        IndexSet::new(),
        &[],
        Val::Tuple(Rc::new(vec![Val::F64(2.), Val::F64(2.)])),
    )
    .unwrap();
    assert_eq!(answer, Val::F64(0.));
}
