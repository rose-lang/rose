import { Real, Vec, fn, generic, get, mul, sub, sum, vec } from ".";

const dot = generic(({ n }) =>
  fn([Vec(Real, n), Vec(Real, n)], (u, v) =>
    sum(vec(n, (i) => mul(get(u, i), get(v, i))))
  )
);

const cross = fn([Vec(Real, 2), Vec(Real, 2)], (u, v) =>
  sub(mul(get(u, 0), get(v, 1)), mul(get(v, 0), get(u, 1)))
);
