import { Real, Vec, compile, fn, jvp, vec, vjp } from "rose";
import { Expr, parse } from "./parse.js";

type Vec2 = [number, number];

interface Info {
  val: number;
  grad: Vec2;
  hess: [Vec2, Vec2];
}

type Func = (x: number, y: number) => Info;

const autodiff = async (root: Expr): Promise<Func> => {
  const Vec2 = Vec(2, Real);
  const f = fn([Vec2], Real, (v) => {
    const emit = (e: Expr): Real => {
      switch (e.kind) {
        case "const":
          return e.val;
        case "var":
          return v[e.idx];
        case "unary":
          return e.f(emit(e.arg));
        case "binary":
          return e.f(emit(e.lhs), emit(e.rhs));
      }
    };
    return emit(root);
  });

  const Mat2 = Vec(2, Vec2);
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

  return (await compile(
    fn([Real, Real], { val: Real, grad: Vec2, hess: Mat2 }, (x, y) => {
      const v = [x, y];
      return { val: f(v), grad: g(v), hess: h(v) };
    }),
  )) as unknown as Func;
};

interface Parabola {
  /** coefficient of square term */
  a: number;
  /** coefficient of linear term */
  b: number;
  /** constant */
  c: number;
}

const xParabola = (
  [a, b]: Vec2,
  { val: f, grad: [fx, fy], hess: [[fxx], [fxy, fyy]] }: Info,
  y: number,
): Parabola => {
  return {
    a: (1 / 2) * fxx,
    b: fx + fxx * -a + fxy * (y - b),
    c:
      f -
      fx * a +
      fy * (y - b) +
      (1 / 2) * fxx * a ** 2 -
      fxy * a * (y - b) +
      (1 / 2) * fyy * (y - b) ** 2,
  };
};

const yParabola = (
  [a, b]: Vec2,
  { val: f, grad: [fx, fy], hess: [[fxx], [fxy, fyy]] }: Info,
  x: number,
): Parabola => {
  return {
    a: (1 / 2) * fyy,
    b: fy + fxy * (x - a) + fyy * -b,
    c:
      f +
      fx * (x - a) +
      fy * -b +
      (1 / 2) * fxx * (x - a) ** 2 +
      fxy * (x - a) * -b +
      (1 / 2) * fyy * b ** 2,
  };
};

interface PointSlope {
  point: Vec2;
  slope: number;
}

const pointSlope = ({ a, b, c }: Parabola, x: number): PointSlope => {
  const y = a * x ** 2 + b * x + c;
  const m = 2 * a * x + b;
  return { point: [x, y], slope: m };
};

const intersectPointSlope = (l1: PointSlope, l2: PointSlope): Vec2 => {
  const [x1, y1] = l1.point;
  const [x2, y2] = l2.point;
  const m1 = l1.slope;
  const m2 = l2.slope;
  const x = (m1 * x1 - m2 * x2 - y1 + y2) / (m1 - m2);
  const y = m1 * (x - x1) + y1;
  return [x, y];
};

const bezier = (
  parabola: Parabola,
  x1: number,
  x2: number,
): [Vec2, Vec2, Vec2] => {
  const l1 = pointSlope(parabola, x1);
  const l2 = pointSlope(parabola, x2);
  let [x3, y3] = intersectPointSlope(l1, l2);
  if (!(Number.isFinite(x3) && Number.isFinite(y3))) {
    const [x1, y1] = l1.point;
    const [x2, y2] = l2.point;
    x3 = (x1 + x2) / 2;
    y3 = (y1 + y2) / 2;
  }
  return [l1.point, [x3, y3], l2.point];
};

type Vec3 = number[];

type Mat3x3 = Vec3[];

const matVecMul = (a: Mat3x3, b: Vec3): Vec3 => {
  const c: Vec3 = [0, 0, 0];
  for (let i = 0; i < 3; ++i) {
    for (let j = 0; j < 3; ++j) {
      c[i] += a[i][j] * b[j];
    }
  }
  return c;
};

const matMul = (a: Mat3x3, b: Mat3x3): Mat3x3 => {
  const c: Mat3x3 = [
    [0, 0, 0],
    [0, 0, 0],
    [0, 0, 0],
  ];
  for (let i = 0; i < 3; ++i) {
    for (let j = 0; j < 3; ++j) {
      for (let k = 0; k < 3; ++k) {
        c[i][j] += a[i][k] * b[k][j];
      }
    }
  }
  return c;
};

const inverse = (a: Mat3x3): Mat3x3 => {
  const [[a00, a01, a02], [a10, a11, a12], [a20, a21, a22]] = a;
  const det =
    a00 * (a11 * a22 - a12 * a21) -
    a01 * (a10 * a22 - a12 * a20) +
    a02 * (a10 * a21 - a11 * a20);
  const b00 = (a11 * a22 - a12 * a21) / det;
  const b01 = -(a01 * a22 - a02 * a21) / det;
  const b02 = (a01 * a12 - a02 * a11) / det;
  const b10 = -(a10 * a22 - a12 * a20) / det;
  const b11 = (a00 * a22 - a02 * a20) / det;
  const b12 = -(a00 * a12 - a02 * a10) / det;
  const b20 = (a10 * a21 - a11 * a20) / det;
  const b21 = -(a00 * a21 - a01 * a20) / det;
  const b22 = (a00 * a11 - a01 * a10) / det;
  return [
    [b00, b01, b02],
    [b10, b11, b12],
    [b20, b21, b22],
  ];
};

const rotateX = (theta: number): Mat3x3 => [
  [1, 0, 0],
  [0, Math.cos(theta), -Math.sin(theta)],
  [0, Math.sin(theta), Math.cos(theta)],
];

const rotateZ = (theta: number): Mat3x3 => [
  [Math.cos(theta), -Math.sin(theta), 0],
  [Math.sin(theta), Math.cos(theta), 0],
  [0, 0, 1],
];

const scalePlane = 130;
const scaleValue = 40;
const tilt = -1;
const screen = [
  rotateX(tilt),
  rotateZ((-3 * Math.PI) / 4),
  [
    [scalePlane, 0, 0],
    [0, scalePlane, 0],
    [0, 0, scaleValue],
  ],
].reduce(matMul);
const world = inverse(screen);

const toScreen = (v: Vec3): Vec2 => {
  const [x, y] = matVecMul(screen, v);
  return [x, y];
};

const toWorld = ([x, y]: Vec2): Vec3 => {
  const [, , [a, b, c]] = world;
  const z = -(a * x + b * y) / c;
  return matVecMul(world, [x, y, z]);
};

let func: Func;
let point: Vec2 = [0.5, 0.5];
let info: Info;

const setPoint = (newPoint: Vec2) => {
  point = newPoint;
  info = func(...point);
};

const textbox = document.getElementById("textbox") as HTMLInputElement;
const setFunc = async () => {
  let root: Expr = { kind: "const", val: NaN };
  try {
    root = parse(textbox.value);
    textbox.classList.remove("error");
  } catch (e) {
    textbox.classList.add("error");
  }
  func = await autodiff(root);
  setPoint(point);
};
await setFunc();
textbox.addEventListener("input", async () => {
  await setFunc();
});

const roseColor = "#C33358";

const canvas = document.getElementById("canvas") as HTMLCanvasElement;
const { width, height } = canvas;
const ctx = canvas.getContext("2d")!;
ctx.lineWidth = 3;
ctx.lineCap = "round";

const update = (u: number, v: number) => {
  const [x, y] = toWorld([u - width / 2, height / 2 - v]);
  setPoint([x, y]);
};

const mouse = (e: MouseEvent) => {
  if (e.buttons & 1) update(e.offsetX, e.offsetY);
};

canvas.addEventListener("mousedown", mouse);
canvas.addEventListener("mousemove", mouse);

const touch = (e: TouchEvent) => {
  e.preventDefault();
  const rect = canvas.getBoundingClientRect();
  const { clientX: x, clientY: y } = e.touches[0];
  update(x - rect.left, y - rect.top);
};

canvas.addEventListener("touchstart", touch);
canvas.addEventListener("touchmove", touch);

const poly = (points: Vec3[]) => {
  ctx.moveTo(...toScreen(points[0]));
  for (let i = 1; i < points.length; ++i) {
    ctx.lineTo(...toScreen(points[i]));
  }
};

const quadratic = (a: Vec3, b: Vec3, c: Vec3) => {
  ctx.moveTo(...toScreen(a));
  ctx.quadraticCurveTo(...toScreen(b), ...toScreen(c));
};

const lineHalf = 0.015;

const arrowLen = 0.1;
const arrowHalf = 0.05;

const draw: FrameRequestCallback = (milliseconds) => {
  ctx.resetTransform();
  ctx.clearRect(0, 0, width, height);

  ctx.translate(width / 2, height / 2);
  ctx.scale(1, -1);

  const pulse = Math.sin(milliseconds / 1000) / 10;

  ctx.fillStyle = "grey";
  ctx.beginPath();
  poly([
    [1 + pulse + arrowLen, 0, 0],
    [1 + pulse, arrowHalf, 0],
    [1 + pulse, lineHalf, 0],
    [lineHalf, lineHalf, 0],
    [lineHalf, 1 + pulse, 0],
    [arrowHalf, 1 + pulse, 0],
    [0, 1 + pulse + arrowLen, 0],
    [-arrowHalf, 1 + pulse, 0],
    [-lineHalf, 1 + pulse, 0],
    [-lineHalf, -lineHalf, 0],
    [1 + pulse, -lineHalf, 0],
    [1 + pulse, -arrowHalf, 0],
  ]);
  ctx.closePath();
  ctx.fill();

  ctx.fillStyle = roseColor;
  ctx.beginPath();
  ctx.ellipse(
    ...toScreen([...point, 0]),
    scalePlane * 0.05,
    scalePlane * 0.05 * Math.cos(tilt),
    0,
    0,
    2 * Math.PI,
  );
  ctx.fill();

  ctx.strokeStyle = roseColor;
  ctx.beginPath();
  ctx.moveTo(...toScreen([...point, 0]));
  ctx.lineTo(...toScreen([...point, info.val]));
  ctx.stroke();

  ctx.fillStyle = roseColor;
  ctx.beginPath();
  ctx.ellipse(
    ...toScreen([...point, info.val]),
    scalePlane * 0.03,
    scalePlane * 0.03,
    0,
    0,
    2 * Math.PI,
  );
  ctx.fill();

  ctx.strokeStyle = "white";
  for (let x = -0.875; x < 0.9; x += 0.25) {
    const parabola = yParabola(point, info, x);
    const y = Math.sqrt((1 + pulse) ** 2 - x ** 2);
    const [[y1, z1], [y2, z2], [y3, z3]] = bezier(parabola, -y, y);
    ctx.beginPath();
    quadratic([x, y1, z1], [x, y2, z2], [x, y3, z3]);
    ctx.stroke();
  }
  for (let y = -0.875; y < 0.9; y += 0.25) {
    const parabola = xParabola(point, info, y);
    const x = Math.sqrt((1 + pulse) ** 2 - y ** 2);
    const [[x1, z1], [x2, z2], [x3, z3]] = bezier(parabola, -x, x);
    ctx.beginPath();
    quadratic([x1, y, z1], [x2, y, z2], [x3, y, z3]);
    ctx.stroke();
  }

  window.requestAnimationFrame(draw);
};

window.requestAnimationFrame(draw);
