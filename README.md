group 57

---

Questions for meeting with TA:
---------------------------------

1. Player en Enemy hebben beide een `Movable`-class, is het beter om een gedeeld Body-record in beide types te stoppen? (pos, speed, hitBox)
2. Is een grote `Engine.hs` een probleem? Want zodra die groter wordt wordt het wel wat onoverzichterlijker.
3. `GameState` heeft zowel `isPaused :: Bool` en `AppMode`, is het beter om `Paused` een `AppMode` te maken? (dan worden ongeldige combi's onmogelijk als het goed is)
4. When creating levels using ASCII, and also loading bitmaps, how can we make sure the bitmaps dont float? And how to translate ASCII coordinates to Gloss coordinates
