import {
  Dual,
  Real,
  Vec,
  add,
  compile,
  div,
  fn,
  jvp,
  mul,
  opaque,
  vec,
  vjp,
} from "rose";

const log = opaque([Real], Real, Math.log);
log.jvp = fn([Dual], Dual, ({ re: x, du: dx }) => {
  return { re: log(x), du: div(dx, x) };
});

const pow = opaque([Real, Real], Real, Math.pow);
pow.jvp = fn([Dual, Dual], Dual, ({ re: x, du: dx }, { re: y, du: dy }) => {
  const z = pow(x, y);
  return { re: z, du: mul(add(mul(dx, div(y, x)), mul(dy, log(x))), z) };
});

const Vec2 = Vec(2, Real);
const Mat2 = Vec(2, Vec2);

const f = fn([Vec2], Real, ([x, y]) => pow(x, y));
const g = fn([Vec2], Vec2, (v) => vjp(f)(v).grad(1));
const h = fn([Vec2], Mat2, ([x, y]) => {
  const d = jvp(g);
  const a = d([
    { re: x, du: 1 },
    { re: y, du: 0 },
  ]);
  const b = d([
    { re: x, du: 0 },
    { re: y, du: 1 },
  ]);
  return [vec(2, Real, (i) => a[i].du), vec(2, Real, (i) => b[i].du)];
});

const all = await compile(
  fn([Real, Real], { val: Real, grad: Vec2, hess: Mat2 }, (x, y) => {
    const v = [x, y];
    return { val: f(v), grad: g(v), hess: h(v) };
  }),
);

console.log(all(2, 3));

const canvas = document.getElementById("canvas") as HTMLCanvasElement;
const { width, height } = canvas;
const ctx = canvas.getContext("2d")!;

const draw: FrameRequestCallback = (milliseconds) => {
  ctx.resetTransform();
  ctx.clearRect(0, 0, width, height);

  ctx.translate(width / 2, height / 2);
  ctx.scale(1, -1);
  ctx.rotate(milliseconds / 1000);

  ctx.fillStyle = "#c7254e";
  ctx.fillRect(-100, -100, 200, 200);

  window.requestAnimationFrame(draw);
};

window.requestAnimationFrame(draw);
