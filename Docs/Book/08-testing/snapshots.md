# Snapshots

Simplify complex object verification by comparing against JSON baselines.

## Why use Snapshots?

Instead of writing dozens of `Should.Be` assertions for a large object, you save the entire object to a JSON file on the first run, and Dext will compare future runs against this "stamped" version.

## Basic Example

```pascal
[Test]
procedure TestComplexResponse;
begin
  var Result := Service.GenerateReport(123);
  
  // First time: creates the .json file
  // Subsequent times: compares if content is identical
  Result.MatchSnapshot;
end;
```

## Where are Snapshots saved?

Files are automatically created in a folder named `__snapshots__` in the same directory as your test file.

```
tests/
├── ReportGeneratorTests.pas
└── __snapshots__/
    └── ReportGeneratorTests.TestComplexResponse.json
```

## Updating Snapshots

If you changed the logic and the new JSON is correct, you can update snapshots via command line:

```bash
dext test --update-snapshots
```

## Ignoring Variable Fields

If your JSON contains fields that change every run (like Timestamps or random IDs), you can ignore them:

```pascal
Result.MatchSnapshot(procedure(Options: TSnapshotOptions)
  begin
    Options.IgnorePaths(['$.GenerationDate', '$.UniqueIdentifier']);
  end);
```

---

[← Assertions](assertions.md) | [Next: CLI →](../09-cli/README.md)
