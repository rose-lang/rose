import all from "./func.js";

console.log(all(2, 3));

type Vec2 = [number, number];
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

const identity: Mat3x3 = [
  [1, 0, 0],
  [0, 1, 0],
  [0, 0, 1],
];

const scale = (s: number): Mat3x3 => [
  [s, 0, 0],
  [0, s, 0],
  [0, 0, s],
];

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

const screen = [scale(130), rotateX(-1), rotateZ((-3 * Math.PI) / 4)].reduce(
  matMul,
);

const toScreen = (v: Vec3): Vec2 => {
  const [x, y] = matVecMul(screen, v);
  return [x, y];
};

const canvas = document.getElementById("canvas") as HTMLCanvasElement;
const { width, height } = canvas;
const ctx = canvas.getContext("2d")!;

const poly = (points: Vec3[]) => {
  ctx.moveTo(...toScreen(points[0]));
  for (let i = 1; i < points.length; ++i) {
    ctx.lineTo(...toScreen(points[i]));
  }
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

  ctx.strokeStyle = "white";
  ctx.lineWidth = 3;
  ctx.lineCap = "round";
  for (let x = -0.875; x < 0.9; x += 0.25) {
    const y = Math.sqrt((1 + pulse) ** 2 - x ** 2);
    ctx.beginPath();
    poly([
      [x, -y, 0],
      [x, y, 0],
    ]);
    ctx.stroke();
  }
  for (let y = -0.875; y < 0.9; y += 0.25) {
    const x = Math.sqrt((1 + pulse) ** 2 - y ** 2);
    ctx.beginPath();
    poly([
      [-x, y, 0],
      [x, y, 0],
    ]);
    ctx.stroke();
  }

  window.requestAnimationFrame(draw);
};

window.requestAnimationFrame(draw);
