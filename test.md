```
sudo -i -u postgres
psql -d repo
```

```
delete from modules where filepath LIKE '%fixtures/SimpleTest.hs';
```

```
stack exec -- ghc -c -fno-code -fforce-recomp -fplugin Language.Haskell.HsTools.Plugin -fplugin-opt Language.Haskell.HsTools.Plugin:'postgresql://saver:saver@127.0.0.1:5432/repo' -fplugin-opt Language.Haskell.HsTools.Plugin:verbose fixtures/SimpleTest.hs
```