import Data.List (break)

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk = Folder "root"
  [
    File "goat_yelling_like_man.wmv" "baaaaaa",
    File "pope_time.avi" "god bless",
    Folder "pics"
      [
        File "ape_throwing_up.jpg" "bleargh",
        File "watermelon_smash.gif" "smash!!",
        File "skull_man(scary).bmp" "Yikes!"
      ],
    File "dijon_poupon.doc" "beat mustard",
    Folder "programs"
      [
        File "fartwizard.exe" "10gotofart",
        File "fowl_bandit.dmg" "mov eax, h00t",
        File "not_a_virus.exe" "really not a virus",
        Folder "source code" 
          [
            File "best_hs_prog.hs" "main = print (fix error)",
            File "random.hs" "main = print 4"
          ]
      ]
  ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])
fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder foldrName _) = name == foldrName
nameIs name (File fileName _) = name == fileName