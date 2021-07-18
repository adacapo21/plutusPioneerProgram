**Plutus Pioneer Program**

**Creator: [Kappos Angelos](https://www.linkedin.com/in/angelos-dionysios-kappos-4b668140/)**

**This project/repository is implemented for educational purposes and it is an unoffocial repo for everything related to the Plutus Pioneer Program.**

The **main repositories** of the Plutus Pioneer Program can be found here:

- [Plutus Pioneer Program Repository](https://github.com/input-output-hk/plutus-pioneer-program) 
- [Plutus Repository](https://github.com/input-output-hk/plutus)

**Overview of repository:**
- Code with explanatory comments,
- Solutions of Homeworks
- Practices, 
- Techincal Documentation for each lecture,  
All this knowledge comes after following the Plutus Pioneer Program of IOHK.
Every Week, the next series of the Program will be added to the repository.
Don't hesitate to contribute if you need is required.

**HOW TO Set up correctly your environment for each week exercise**:

To your locally installed Plutus repository:
```
cd cardano/plutus 
git pull
git checkout 219992289c6615e197069d022735cb4059d43229 
-- here add the tag from cabal.project of each week exercise
-- this tag can be found in file cabal.project in week03 of plutus-pioneer-program repository--
```
**Build Plutus && start server/client:** 

After pull latest changes in your local environmentm you need to build it before you start your server and client. 
You should follow those lines one by one, in order to have a succesful build:
```
nix build -f default.nix plutus.haskell.packages.plutus-core 
nix-build -A plutus-playground.client 
nix-build -A plutus-playground.server 
nix-build -A plutus-playground.generate-purescript 
nix-build -A plutus-playground.start-backend 
nix-build -A plutus-pab 
nix-shell 
cd plutus-pab 
plutus-pab-generate-purs 
cd ../plutus-playground-server 
plutus-playground-generate-purs
```

Start Server
```
plutus-playground-server
```
In a new Terminal
```
cd cardano/plutus
nix-shell
cd plutus-playground-client
```
and start the Client (this may take a while)
```
npm run start
```
in Browser
```
https://localhost:8009/
```
In a new Terminal again:
```
cd cardano/plutus-pioneer-program
git fetch
cd ../plutus
nix-shell
cd  ../plutus-pioneer-program/code/week03/
cabal update  
cabal build
```

Some **LINKS** that you can check, are listed below:

**Video Lectures**:

- [Week 1](https://www.youtube.com/watch?v=_zr3W8cgzIQ&t=2210s&ab_channel=LarsBr%C3%BCnjes)
- [Week 2](https://www.youtube.com/watch?v=sN3BIa3GAOc&ab_channel=LarsBr%C3%BCnjes)
- [Week 3](https://www.youtube.com/watch?v=6_rfCCY9_gY&t=52s&ab_channel=LarsBr%C3%BCnjes)

**Linkedin** :
[Kappos Angelos](https://www.linkedin.com/in/angelos-dionysios-kappos-4b668140/)

**Twitter accounts:**
- [angelokappos](https://twitter.com/angelokappos)
- [sapiopool](https://twitter.com/sapiopool)

**Who is SapioPool Cardano Community** : [Sapiopool Website](https://sapiopool.com/)


**Participate/collaborate in Sapiopool Greek Community *Discord* channel here** :
- [Discord Sapiopool](https://discord.com/invite/HRK9gGE9ax)

**Youtube:**
- [Sapiopool](https://www.youtube.com/channel/UCcPH2RMsszRGJ2awvLdMKzQ)
- [Marlowe](https://www.youtube.com/user/simonjohnthompson/videos)
- [Haskell](https://www.youtube.com/playlist?list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV)

**Haskell**:
[Learn Haskell](http://learnyouahaskell.com/chapters)
- http://learnyouahaskell.com/chapters

