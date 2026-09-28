# Haskell style guide

Conventions for Haskell code in this project. These apply to hand-written
code (libraries, executables, test suites, benchmarks). Generated code
(FFI bindings, code emitted by Template Haskell or external generators,
vendored sources) is out of scope for style review. If this project has
generated code, list its location here: `<path/to/generated/>`.

## Syntax preferences

**`$` over nested parens.** Prefer `f $ g a` to `f (g a)`. Chain with `$`
rather than nesting parens when the argument is the last thing being built.

```haskell
-- prefer
putStrLn $ unwords $ map show $ filter even xs

-- over
putStrLn (unwords (map show (filter even xs)))
```

**Type applications over `:: T` annotations.** When you need to pin a type
to help inference, prefer `f @T x` to `(f x :: T)` or to annotating an
intermediate binding.

```haskell
-- prefer
n = read @Int input
total = fromIntegral @Word8 @Int byte + offset

-- over
n = read input :: Int
total = (fromIntegral byte :: Int) + offset
```

Caveat: a type application binds type variables in the order they are
quantified, which may not be the order you want. `realToFrac :: (Real a,
Fractional b) => a -> b` quantifies `a` before `b`, so `realToFrac @Double`
pins the *input* type, not the result. Check which variable you are actually
fixing before you use this. If the variable you need to pin isn't first, you
have two options. You can add an explicit `forall` at the definition site (for
functions you own) so that type applications read naturally at call sites. Or
you can use `@_` to skip a variable (`realToFrac @_ @Double`). If neither works
cleanly, use an annotation. A correct `::` is better than a wrong `@`.

Use `:: T` only when there is no type variable to apply the type to, such as
when disambiguating a numeric literal or annotating a pattern or expression
whose head has no usable `forall`.

**`OverloadedStrings` over manual `pack`.** When you build a `Text` or
`ByteString` from a string *literal*, enable `OverloadedStrings` and write the
literal directly instead of calling `T.pack`/`BS8.pack` on it.

```haskell
-- prefer
{-# LANGUAGE OverloadedStrings #-}
name = "my-project"

-- over
name = T.pack "my-project"
```

This rule covers *literals* only. Converting a runtime `String` (a line read
from stdin, a `show` result, an environment variable) is a real conversion
that `OverloadedStrings` does not replace, so `T.pack`/`BS8.pack` are still
correct there.

**Prefer `\case` (`LambdaCase`) when a function's whole argument is
scrutinized immediately.** This replaces `\x -> case x of ...`. It does not
apply to ordinary multi-equation definitions, which are already idiomatic.
It also does not apply to a `case` over an expression *derived from* the
argument (`case f x of ...`). `\case` only removes the redundant
`\x ->`/`case x of` pair.

```haskell
-- prefer
describe = \case
    Nothing -> "nothing"
    Just x  -> "just " <> show x

-- over
describe x = case x of
    Nothing -> "nothing"
    Just y  -> "just " <> show y
```

## Types over checks

**Make invalid states unrepresentable.** Use a sum type or enum instead of a
manual check (a boolean flag, a magic number, a string tag) whenever the set
of valid states is known in advance. Pattern-match exhaustively on the type
instead of branching on a derived condition.

```haskell
-- prefer
data Status = Pending | Active | Closed

handle :: Status -> IO ()
handle = \case
    Pending -> ...
    Active  -> ...
    Closed  -> ...

-- over
handle :: Int -> IO ()
handle s
    | s == 0    = ...
    | s == 1    = ...
    | otherwise = ...
```

When an external source (C enums, wire formats, database columns) gives you
raw tags, convert them into a proper type at the boundary. You can use a
parsing function or pattern synonyms. The rest of the code then matches on
constructors and never compares raw values.

## Simplicity

**Inline simple, single-use expressions instead of let-floating them.** A
`let`/`where` binding that is used once and has a short right-hand side
usually reads better inlined at its use site. Use a binding when the value is
reused, when naming it clarifies intent, or when inlining a long expression
would hurt readability. Don't create one by default.

**Deriving over manual instances.** Prefer `deriving`, `deriving stock`,
`deriving newtype`, `deriving anyclass`, and `deriving via` (with
`DerivingStrategies`) to hand-written instances wherever derivation is
available. If a type is *almost* derivable (a record needs reshaping, a
newtype needs its representation adjusted, a class needs a `Generic`-friendly
shape), it is fine to ask for that small refactor instead of hand-writing the
instance around the awkward shape. A trivially derivable type is worth more
than a type that avoids one field reorder.

## Custom monads: always provide an escape hatch

A bespoke monad (`AppM a`) should not be the *only* way to get its effects.
Alongside the concrete monad:

1. Expose a `MonadApp m` typeclass that captures its operations.
2. Give the concrete monad a `MonadApp` instance.
3. Write the rest of the API (helpers, marshalling, everything downstream)
   against the `MonadApp m` constraint, not the concrete type.

The concrete monad is then a ready-to-use convenience instance, not a hard
dependency. Callers who want to embed these effects in their own transformer
stack write one instance and are not locked out.

```haskell
class MonadIO m => MonadApp m where
    askAppEnv :: m AppEnv
    tryApp    :: m a -> m (Either AppException a)

newtype AppM a = AppM (ReaderT AppEnv IO a)
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadApp AppM where
    ...

doThing :: MonadApp m => Text -> m Result
```

The exception is functions that own the monad's whole lifecycle, such as
acquiring and releasing resources, bracketing, and running it (`runAppM`,
`runAppMEither`). Those stay concrete.

## Testing

**Prefer property testing (Hedgehog) over verbose unit testing.** Unit tests
are still allowed. A specific regression, a fixed example from a bug report,
or a case that is awkward to generate is still worth a direct unit test. But
default to a property test when the code under test has a law, invariant, or
round trip to state, for example:

- `decode . encode == Right`
- parser/printer round trips
- typeclass laws
- "no input of this shape crashes"

Don't write a long list of example assertions that a generator would cover
more thoroughly with less code. Watch for five near-identical unit tests that
differ only in their input literal. That is almost always a property test
that hasn't been recognized as one yet.

## Project management with Nix flakes

Every project is built, developed, and checked through a `flake.nix` at the
repository root. Nix is the source of truth for the toolchain. Do not rely on
globally installed GHC, cabal, HLS, or formatters.

**Requirements**

- **Commit `flake.lock`.** Change it on purpose with `nix flake update` (or
  `nix flake update <input>`), in its own commit, never as a side effect.
- **Pin one GHC version** explicitly (`pkgs.haskell.packages.ghc<NNN>`).
  Don't use the moving `haskellPackages` default, so that nixpkgs bumps don't
  silently change the compiler.
- **Standard outputs.** Every flake exposes:
  - `packages.default`: the project, built with `nix build`.
  - `devShells.default`: GHC plus `cabal-install`,
    `haskell-language-server`, `hlint`, and the formatter, via
    `nix develop`.
  - `checks`: at least the package build (which runs the test suite), plus
    any lint or format checks. Run them with `nix flake check`.
  - `formatter`: for the Nix files, via `nix fmt`.
  - `apps.default` for projects with an executable, via `nix run`.
- **The `.cabal` file is still authoritative** for Haskell dependencies. Nix
  builds it (via `callCabal2nix` or haskell.nix) and does not duplicate the
  dependency list. Put dependency overrides (a newer version, `doJailbreak`,
  `dontCheck`) in the flake's package-set overlay, each with a comment saying
  why.
- **`nix flake check` must pass before merging.** CI runs exactly this, so
  that local and CI results match.
- **Use direnv** (`echo "use flake" > .envrc`) so that the dev shell loads
  automatically. Commit `.envrc` and add `.direnv/` to `.gitignore`.
- **Add non-Haskell system dependencies** (C libraries, `pkg-config`, CLI
  tools used by tests) to the flake, not to a README telling people to
  install them.
- **Keep new files visible to Nix.** Flakes only see files tracked by git,
  so run `git add` on new files before `nix build` or `nix flake check`.

**Starter `flake.nix`**

```nix
{
  description = "my-project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Pin the compiler explicitly.
        hsPkgs = pkgs.haskell.packages.ghc910.override {
          overrides = hfinal: hprev: {
            # Dependency overrides go here, each with a reason, e.g.:
            # some-dep = pkgs.haskell.lib.doJailbreak hprev.some-dep; # upper bound too tight
          };
        };

        my-project = hsPkgs.callCabal2nix "my-project" ./. { };
      in
      {
        packages.default = my-project;

        apps.default = flake-utils.lib.mkApp { drv = my-project; };

        devShells.default = hsPkgs.shellFor {
          packages = _: [ my-project ];
          withHoogle = true;
          nativeBuildInputs = [
            hsPkgs.cabal-install
            hsPkgs.haskell-language-server
            hsPkgs.hlint
            hsPkgs.fourmolu
          ];
        };

        checks = {
          build = my-project;
          hlint = pkgs.runCommand "hlint" { nativeBuildInputs = [ hsPkgs.hlint ]; } ''
            hlint ${./.}/src ${./.}/app ${./.}/test
            touch $out
          '';
        };

        formatter = pkgs.nixfmt-rfc-style;
      });
}
```

Adjust the GHC version, source directories, and `apps.default` (remove it for
library-only projects) for each project.

**Everyday commands**

| Task                    | Command                    |
| ----------------------- | -------------------------- |
| Enter the dev shell     | `nix develop` (or direnv)  |
| Build                   | `nix build`                |
| Run the executable      | `nix run`                  |
| Run all checks          | `nix flake check`          |
| Format Nix files        | `nix fmt`                  |
| Update pinned inputs    | `nix flake update`         |
| Fast iteration in shell | `cabal build` / `cabal test` / `cabal repl` |

