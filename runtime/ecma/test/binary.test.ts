import { describe, expect, it } from "vitest";
import { matmul } from "../python";

describe("Matrix multiplication (matmul)", () => {
  it("should multiply two 2x2 matrices correctly", () => {
    const A = [
      [1, 2],
      [3, 4],
    ];

    const B = [
      [5, 6],
      [7, 8],
    ];

    const result = matmul(A, B);
    expect(result).toEqual([
      [19, 22],
      [43, 50],
    ]);
  });

  it("should throw error when matrices dimensions do not match", () => {
    const A = [
      [1, 2],
      [3, 4],
    ];

    const B = [[1, 2]];

    expect(() => matmul(A, B)).toThrow(
      "Matrix dimensions do not match for multiplication",
    );
  });

  it("should multiply 1xN and Nx1 matrices correctly", () => {
    const A = [[1, 2, 3]];
    const B = [[4], [5], [6]];

    const result = matmul(A, B);
    expect(result).toEqual([[32]]);
  });

  it("should return an empty result for multiplying empty matrices", () => {
    const A: number[][] = [];
    const B: number[][] = [];

    expect(() => matmul(A, B)).toThrow("Cannot multiply empty matrices");
  });
});
