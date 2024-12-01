// For a detailed explanation regarding each configuration property, visit:
// https://jestjs.io/docs/en/configuration.html

/** @type {import('jest').Config} */
export default {
  collectCoverage: true,
  coverageDirectory: "coverage",
  roots: ["<rootDir>/lib/es6/"],
  testEnvironment: "node",
  testMatch: ["**/__tests__/*.res.mjs"],
  transform: {},
};
