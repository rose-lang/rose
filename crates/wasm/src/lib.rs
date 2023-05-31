#[cfg(test)]
mod tests {
    use wasmtime::{Caller, Engine, Func, Instance, Module, Store};

    // https://docs.rs/wasmtime/9.0.2/wasmtime/
    #[test]
    fn test_foo() -> wasmtime::Result<()> {
        // Modules can be compiled through either the text or binary format
        let engine = Engine::default();
        let wat = r#"
        (module
            (import "host" "host_func" (func $host_hello (param i32)))

            (func (export "hello")
                i32.const 3
                call $host_hello)
        )
    "#;
        let module = Module::new(&engine, wat)?;

        // All wasm objects operate within the context of a "store". Each
        // `Store` has a type parameter to store host-specific data, which in
        // this case we're using `4` for.
        let mut store = Store::new(&engine, 4);
        let host_func = Func::wrap(&mut store, |caller: Caller<'_, u32>, param: i32| {
            println!("Got {} from WebAssembly", param);
            println!("my host state is: {}", caller.data());
        });

        // Instantiation of a module requires specifying its imports and then
        // afterwards we can fetch exports by name, as well as asserting the
        // type signature of the function with `get_typed_func`.
        let instance = Instance::new(&mut store, &module, &[host_func.into()])?;
        let hello = instance.get_typed_func::<(), ()>(&mut store, "hello")?;

        // And finally we can call the wasm!
        hello.call(&mut store, ())?;

        Ok(())
    }
}
