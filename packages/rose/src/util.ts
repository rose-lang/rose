export const unreachable = (): never => {
  throw Error("unreachable");
};

/**
 * Throws if `x === undefined`.
 * @returns `x`
 * @param f called if `x === undefined`, to produce error message
 */
export const unwrap = <T>(x: T | undefined, f?: () => string): T => {
  if (x === undefined)
    throw Error((f ?? (() => "called `unwrap` with `undefined`"))());
  return x;
};
