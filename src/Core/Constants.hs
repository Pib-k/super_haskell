module Core.Constants where

import Core.Types (Position)

moveSpeed, jumpSpeed, fastFallSpeed :: Float
moveSpeed = 200
jumpSpeed = 400
fastFallSpeed = -300

gravity :: Float
gravity = -900

voidY :: Float
voidY = -400

maxStep :: Float
maxStep = 1 / 60

walkSpeed :: Float
walkSpeed = 60

bounceSpeed, explosionDuration :: Float
bounceSpeed = 300
explosionDuration = 0.5

stompScore, explosionFrames :: Int
stompScore = 100
explosionFrames = 8

spawnPosition :: Position
spawnPosition = (0, 0)

padding :: Float
padding = 1.0

particleCount :: Int
particleCount = 8

explosionRadius, particleSize :: Float
explosionRadius = 40
particleSize = 5
