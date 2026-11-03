module Core.Constants where

import Core.Types (BoundingBox, Position)

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

bounceSpeed, explosionDuration, mainDoAnimationDuration :: Float
bounceSpeed = 300
explosionDuration = 0.5
mainDoAnimationDuration = 5.0

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

collectibleSize :: BoundingBox
collectibleSize = (20, 20)

collectibleScore :: Int
collectibleScore = 50

screenHalfWidth, levelWidth, sectionWidth, screenHalfHeight :: Float
screenHalfWidth = 400
levelWidth = 2400
sectionWidth = 800
screenHalfHeight = 300

flagThickness, flagHeight :: Float
flagThickness = 26
flagHeight = 120

maxHealth :: Int
maxHealth = 3
levelPlayerBox :: BoundingBox
levelPlayerBox = (30, 30)

menuItems :: [String]
menuItems = ["Start Game", "Level Select", "Controls"]

hudScale, hudSpacing, hudMargin, hudLabelWidth :: Float
hudScale = 0.2
hudSpacing = 65
hudMargin = 25
hudLabelWidth = 120
