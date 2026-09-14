import fs from "node:fs/promises";
import path from "node:path";
import { SpreadsheetFile, Workbook } from "@oai/artifact-tool";

function parseArgs(argv) {
  const args = {};
  for (let index = 0; index < argv.length; index += 2) {
    const key = argv[index];
    const value = argv[index + 1];
    if (!key?.startsWith("--") || value === undefined) {
      throw new Error(`Invalid argument near ${key ?? "end of command"}`);
    }
    args[key.slice(2)] = value;
  }
  return args;
}

function columnLetter(columnNumber) {
  let number = columnNumber;
  let result = "";
  while (number > 0) {
    const remainder = (number - 1) % 26;
    result = String.fromCharCode(65 + remainder) + result;
    number = Math.floor((number - 1) / 26);
  }
  return result;
}

function typedMatrix(matrix, { numeric = [], dates = [], booleans = [], binaryBooleans = [] }) {
  const headers = matrix[0].map(String);
  const numericSet = new Set(numeric);
  const dateSet = new Set(dates);
  const booleanSet = new Set(booleans);
  const binaryBooleanSet = new Set(binaryBooleans);

  return [
    headers,
    ...matrix.slice(1).map((row) =>
      row.map((value, columnIndex) => {
        if (value === "" || value === null || value === undefined) return null;
        const header = headers[columnIndex];
        if (binaryBooleanSet.has(header)) {
          const normalized = String(value).toLowerCase();
          if (normalized === "true" || normalized === "1") return 1;
          if (normalized === "false" || normalized === "0") return 0;
          throw new Error(`Expected TRUE/FALSE in ${header}; found ${value}`);
        }
        if (numericSet.has(header)) {
          const number = Number(value);
          if (!Number.isFinite(number)) {
            throw new Error(`Expected a number in ${header}; found ${value}`);
          }
          return number;
        }
        if (dateSet.has(header)) {
          const date = new Date(`${value}T00:00:00Z`);
          if (Number.isNaN(date.getTime())) {
            throw new Error(`Expected a date in ${header}; found ${value}`);
          }
          return date;
        }
        if (booleanSet.has(header)) {
          const normalized = String(value).toLowerCase();
          if (normalized === "true") return true;
          if (normalized === "false") return false;
          throw new Error(`Expected TRUE/FALSE in ${header}; found ${value}`);
        }
        return String(value);
      }),
    ),
  ];
}

async function csvMatrix(csvPath, sheetName) {
  const csvText = await fs.readFile(csvPath, "utf8");
  const imported = await Workbook.fromCSV(csvText, { sheetName });
  return imported.worksheets.getItem(sheetName).getUsedRange().values;
}

const args = parseArgs(process.argv.slice(2));
const repoRoot = path.resolve(args["repo-root"] ?? ".");
const outputPath = path.resolve(
  args.output ?? path.join(repoRoot, "week02", "labs", "data", "Lab_02_Excel_Starter.xlsx"),
);
const previewDir = args["preview-dir"] ? path.resolve(args["preview-dir"]) : null;

const playsCsv = path.join(repoRoot, "week02", "labs", "data", "nfl_plays.csv");
const gamesCsv = path.join(repoRoot, "week02", "labs", "data", "nfl_games.csv");

const rawPlays = await csvMatrix(playsCsv, "Plays");
const rawGames = await csvMatrix(gamesCsv, "Games");

const plays = typedMatrix(rawPlays, {
  numeric: [
    "play_id", "quarter", "down", "yards_to_go", "yardline_100",
    "yards_gained", "air_yards", "yards_after_catch", "penalty_yards",
    "expected_points_added",
    "win_probability_added",
  ],
  binaryBooleans: [
    "shotgun", "no_huddle", "complete_pass", "first_down", "sack",
    "interception", "fumble_lost", "penalty", "touchdown",
    "successful_play",
  ],
});
const games = typedMatrix(rawGames, {
  numeric: [
    "season", "week", "away_score", "home_score", "home_score_margin",
    "total_points", "temperature_fahrenheit", "wind_mph",
  ],
  dates: ["game_date"],
  binaryBooleans: ["overtime"],
});

if (games.length !== 18 || plays.length !== 2792) {
  throw new Error(`Unexpected Lab 2 source size: ${games.length - 1} games and ${plays.length - 1} plays`);
}

const gameIds = games.slice(1).map((row) => row[0]);
if (new Set(gameIds).size !== gameIds.length) {
  throw new Error("Games game_id values must be unique for XLOOKUP.");
}
const missingGameIds = [...new Set(plays.slice(1).map((row) => row[0]))]
  .filter((gameId) => !gameIds.includes(gameId));
if (missingGameIds.length > 0) {
  throw new Error(`Plays contains unmatched game_id values: ${missingGameIds.join(", ")}`);
}

const workbook = Workbook.create();
const playsSheet = workbook.worksheets.add("Plays");
const gamesSheet = workbook.worksheets.add("Games");

const originalPlayHeaders = plays[0];
// game_id is already the first Plays column and is the lookup key. Append all
// 17 remaining Games columns so every Games field is represented once without
// creating a duplicate header that Excel would rename when students add a Table.
const lookupFields = games[0].slice(1).map((header, index) => ({
  header,
  gamesColumn: columnLetter(index + 2),
}));
const formulaStartIndex = originalPlayHeaders.length;
const firstFormulaColumn = columnLetter(formulaStartIndex + 1);
const lastFormulaColumn = columnLetter(formulaStartIndex + lookupFields.length);

playsSheet.getRange("A1").write(plays);
gamesSheet.getRange("A1").write(games);
playsSheet
  .getRange(`${firstFormulaColumn}1:${lastFormulaColumn}1`)
  .values = [lookupFields.map((field) => field.header)];

lookupFields.forEach((field, index) => {
  const column = columnLetter(formulaStartIndex + index + 1);
  playsSheet.getRange(`${column}2`).formulas = [[
    `=_xlfn.XLOOKUP($A2,Games!$A$2:$A$18,Games!$${field.gamesColumn}$2:$${field.gamesColumn}$18,"Missing game",0)`,
  ]];
  playsSheet.getRange(`${column}2:${column}${plays.length}`).fillDown();
});

// Keep the workbook plain so students can create the Excel Tables and
// PivotTable themselves. Date display formatting is the only formatting.
playsSheet.getRange(`AJ2:AJ${plays.length}`).format.numberFormat = "mm/dd/yy";
gamesSheet.getRange(`E2:E${games.length}`).format.numberFormat = "mm/dd/yy";

workbook.recalculate();

const sheetInspection = await workbook.inspect({
  kind: "sheet",
  include: "id,name,range",
  maxChars: 4000,
});
console.log(sheetInspection.ndjson);
for (const range of [
  `${firstFormulaColumn}1:${lastFormulaColumn}3`,
  `${firstFormulaColumn}1396:${lastFormulaColumn}1397`,
  `${firstFormulaColumn}2791:${lastFormulaColumn}2792`,
]) {
  const formulaInspection = await workbook.inspect({
    kind: "formula",
    sheetId: "Plays",
    range,
    maxChars: 4000,
  });
  console.log(formulaInspection.ndjson);
}
const errorInspection = await workbook.inspect({
  kind: "match",
  searchTerm: "#REF!|#DIV/0!|#VALUE!|#NAME\\?|#N/A|#NUM!|#NULL!|#SPILL!|#CALC!",
  options: { useRegex: true, maxResults: 100 },
  summary: "final formula error scan",
  maxChars: 5000,
});
console.log(errorInspection.ndjson);

if (playsSheet.tables.items.length !== 0 || gamesSheet.tables.items.length !== 0) {
  throw new Error("The starter workbook must not contain Excel Tables.");
}

await fs.mkdir(path.dirname(outputPath), { recursive: true });
const output = await SpreadsheetFile.exportXlsx(workbook);
await output.save(outputPath);

if (previewDir) {
  await fs.mkdir(previewDir, { recursive: true });
  const playsPreview = await workbook.render({
    sheetName: "Plays",
    range: `AA1:${lastFormulaColumn}18`,
    scale: 1,
    format: "png",
  });
  const gamesPreview = await workbook.render({
    sheetName: "Games",
    range: "A1:R18",
    scale: 1,
    format: "png",
  });
  await fs.writeFile(path.join(previewDir, "plays.png"), new Uint8Array(await playsPreview.arrayBuffer()));
  await fs.writeFile(path.join(previewDir, "games.png"), new Uint8Array(await gamesPreview.arrayBuffer()));
}

console.log(JSON.stringify({
  status: "created",
  output: outputPath,
  sheets: ["Plays", "Games"],
  games: games.length - 1,
  plays: plays.length - 1,
  xlookup_columns: lookupFields.map((field) => field.header),
  tables: playsSheet.tables.items.length + gamesSheet.tables.items.length,
  pivot_tables: 0,
}));
