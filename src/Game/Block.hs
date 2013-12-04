module Game.Block where

import           Game.Types
import           Graphics.Rendering.OpenGL


colorForBlock :: Block -> Color4 GLfloat
colorForBlock b = c
    where Just c = lookup (_blockType b) table
          table  = zip types colors
          types  = blockTypes
          colors = [ Color4 0.0 1.0 1.0 1.0
                   , Color4 0.0 0.0 1.0 1.0
                   , Color4 1.0 0.5 0.0 1.0
                   , Color4 1.0 1.0 0.0 1.0
                   , Color4 0.0 1.0 0.0 1.0
                   , Color4 0.5 0.0 0.5 1.0
                   , Color4 1.0 0.0 0.0 1.0
                   ]


blockTypes :: [BlockType]
blockTypes = [TI,TJ,TL,TO,TS,TT,TZ]


boardSize :: (GLfloat, GLfloat)
boardSize = (blockWidth*10, blockWidth*20)


blockWidth :: GLfloat
blockWidth = 20


newBlockWithType :: BlockType -> Block
newBlockWithType TI = blockI
newBlockWithType TJ = blockJ
newBlockWithType TL = blockL
newBlockWithType TO = blockO
newBlockWithType TS = blockS
newBlockWithType TT = blockT
newBlockWithType TZ = blockZ


blockI :: Block
blockI = Block TI (0,0) [[True, True, True, True]]


blockJ :: Block
blockJ = Block TJ (0,0) [[False, True]
                        ,[False, True]
                        ,[True,  True]
                        ]


blockL :: Block
blockL = Block TL (0,0) [[True, False]
                        ,[True, False]
                        ,[True, True]
                        ]

blockO :: Block
blockO = Block TO (0,0) [[True, True]
                        ,[True, True]
                        ]


blockS :: Block
blockS = Block TS (0,0) [[False, True, True]
                        ,[True, True, False]
                        ]


blockT :: Block
blockT = Block TT (0,0) [[True,True,True]
                        ,[False,True,False]
                        ]


blockZ :: Block
blockZ = Block TZ (0,0) [[True, True, False]
                        ,[False, True, True]
                        ]

