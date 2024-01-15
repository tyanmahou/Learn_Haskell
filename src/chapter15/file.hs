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