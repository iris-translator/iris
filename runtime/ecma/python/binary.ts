import { describe, it, expect } from "vitest";

// Matrix multiplication function (same as before)
export function matmul(A: number[][], B: number[][]): number[][] {
  // Check size
  if (A.length === 0 || B.length === 0) {
    throw new Error("Cannot multiply empty matrices");
  }

  const rowsA = A.length;
  const colsA = A[0].length;
  const rowsB = B.length;
  const colsB = B[0].length;

  // Check if the matrices can be multiplied
  if (colsA !== rowsB) {
    throw new Error("Matrix dimensions do not match for multiplication");
  }

  // Create a result matrix with appropriate dimensions
  const result: number[][] = new Array(rowsA)
    .fill(null)
    .map(() => new Array(colsB).fill(0));

  // Perform matrix multiplication
  for (let i = 0; i < rowsA; i++) {
    for (let j = 0; j < colsB; j++) {
      for (let k = 0; k < colsA; k++) {
        result[i][j] += A[i][k] * B[k][j];
      }
    }
  }

  return result;
}
