# **Plutus Pioneer Program**

### **Creator: [Kappos Angelos](https://www.linkedin.com/in/angelos-dionysios-kappos-4b668140/)**
#### Credits to: [Sapiopool Cardano Community](https://sapiopool.com/) && [Discord Sapiopool](https://discord.com/invite/HRK9gGE9ax)
**This project/repository is implemented for educational purposes and it is an unoffocial repo for everything related to the Plutus Pioneer Program.**

The **main repositories** of the Plutus Pioneer Program can be found here:

- [Plutus Pioneer Program Repository](https://github.com/input-output-hk/plutus-pioneer-program) 
- [Plutus Repository](https://github.com/input-output-hk/plutus)
- [Plutus Community Documentation](https://docs.plutus-community.com/)

**Overview of repository:**
- Code with explanatory comments,
- Solutions of Homeworks
- Practices, 
- Techincal Documentation for each lecture,
- Technical Papers and external Links to useful sources
- Further resources for learning (Haskell-Plutus)

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
- [nixos/nix image prepared for Plutus development](https://github.com/hkvb/nixos)

Some **LINKS** that you can check, are listed below:

## Video Lectures:

- [Week 1](https://www.youtube.com/watch?v=_zr3W8cgzIQ&t=2210s&ab_channel=LarsBr%C3%BCnjes)
- [Week 2](https://www.youtube.com/watch?v=sN3BIa3GAOc&ab_channel=LarsBr%C3%BCnjes)
- [Week 3](https://www.youtube.com/watch?v=6_rfCCY9_gY&t=52s&ab_channel=LarsBr%C3%BCnjes)
- [Week 4](https://www.youtube.com/watch?v=g4lvA14I-Jg&t=2422s&ab_channel=LarsBr%C3%BCnjes)
- [Week 5](https://www.youtube.com/watch?v=SsaVjSsPPcg&t=2s&ab_channel=LarsBr%C3%BCnjes)
- [Week 6](https://www.youtube.com/watch?v=24SHPHEc3zo&t=2s&ab_channel=LarsBr%C3%BCnjes)
- [Week 7](https://www.youtube.com/watch?v=uwZ903Zd0DU&t=1s&ab_channel=LarsBr%C3%BCnjes)
## Q&A:
- [Plutus Pioneers second cohort: Q and A with Lars #001](https://zoom.us/rec/share/QU7jcp8S0A-xlmXMIKlua5GoTpL0YwofDlnhSmzgZh1PeFSKqSn9YtzqIGmb6TvC.qZiLZ3thyTOy8dQ0) 
- [Plutus Pioneers second cohort: Q and A with Lars #002](https://zoom.us/rec/share/mfAfBcg8LfGstgdkvXYyP9BVUDiBzm1O_ED9QaTobbcuPzOXOZzGpGBndhr2CauU.bgV5nicKWogoT3Kj) 
- [Plutus Pioneers: Q and A with Lars #003](https://zoom.us/rec/share/7KKN1jsJIEe7hID4nxqo5cc59DQgucNTcXi7evD81cIsanLtJJfz4ymFYj7PW1PE.YajSv6SIKsCBRHKl) 
- [Plutus Pioneers: Q and A with Lars #004](https://zoom.us/rec/share/Gn20cMXHJbmdXG6R0PncZfs2feUuxP9KJjFGQ5oecCu-dvi2KSb-asX_LVSaKRHT.S2R1Eo2ulCeTWDUt
  ) 
 - [Plutus Pioneers: Q and A with Lars #005](https://zoom.us/rec/share/4lASBU00my7lL5l0enCqZVWL7HBHJAI9x4_rJCZAoTaMz58c3C2lSgNTWF8tcB_E.owc7Yk9PGgGCH7c0)  

## Useful Links && Extra Repositiries
- [Cardano Developer Portal](https://developers.cardano.org/?utm_source=Developers&utm_campaign=e6f14df50a-EMAIL_CAMPAIGN_2021_03_31_06_14_COPY_01&utm_medium=email&utm_term=0_c9c99d4ad3-e6f14df50a-77674502)
- [Plutus Community Documentation](https://docs.plutus-community.com/)
- [Smart Contracts Architecture](https://testnets.cardano.org/en/virtual-machines/kevm/about/iele_vm_architecture/)
- [Cardano Stack Exchange](https://cardano.stackexchange.com/)
- [Repository about Cardano Dev from @proyecto26](https://github.com/proyecto26/cardano-developer)
- [Lectures Notes Cohort 1](https://plutus-pioneer-program.readthedocs.io/en/latest/index.html)
- [Alonzo Walkthrough](https://plutus-pioneer-program.readthedocs.io/en/latest/alonzo.html)
- [Plutus Repo from @mputz86](https://github.com/mputz86/plutus-personal-playground)
- [Plutus Repo from @franleplant - Week 7 Solution Structure (Validator - Redeemer - Datum - GameChoice - Test)](https://github.com/franleplant/plutus-pioneer-program-fork/tree/rock-paper-scissors/code/week07/src)  
- [Plutus Lectures Notes](https://plutus-pioneer-program.readthedocs.io/en/latest/)
- [Cardano Developers Reddit](https://www.reddit.com/r/CardanoDevelopers/)

**Youtube:**
- [Sapiopool](https://www.youtube.com/channel/UCcPH2RMsszRGJ2awvLdMKzQ)
- [Marlowe](https://www.youtube.com/user/simonjohnthompson/videos)
- [Haskell](https://www.youtube.com/playlist?list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV)
- [Haskell and Crypto Mongolia 2020](https://www.youtube.com/watch?v=EoO76YCSTLo&list=PLJ3w5xyG4JWmBVIigNBytJhvSSfZZzfTm&ab_channel=AlejandroGarcia)
- [Cardano Developer Portal](https://developers.cardano.org/)

**Haskell**:
- [Learn Haskell](http://learnyouahaskell.com/chapters)

## Extra Reads - Useful Papers
- [Academic Papers](https://github.com/jonathondilworth/blockchain-notes/blob/main/Academic-Papers-List.md)
- [Extended UTXO Model](https://iohk.io/en/research/library/papers/the-extended-utxo-model/)
- [UTxO- vs account-based smart contract blockchain programming paradigms ](https://arxiv.org/pdf/2003.14271.pdf)
- [Native Custom Tokens in the Extended UTXO Model](https://files.zotero.net/eyJleHBpcmVzIjoxNjI3NDc1NTI4LCJoYXNoIjoiMDBmMTM0NGZkYTg2ZTBhOWJkZWI4ZDhhYjIzZjIzYzAiLCJjb250ZW50VHlwZSI6ImFwcGxpY2F0aW9uXC9wZGYiLCJjaGFyc2V0IjoiIiwiZmlsZW5hbWUiOiJDaGFrcmF2YXJ0eSBldCBhbC4gLSAyMDIwIC0gTmF0aXZlIEN1c3RvbSBUb2tlbnMgaW4gdGhlIEV4dGVuZGVkIFVUWE8gTW9kZWwucGRmIn0%3D/bc922137cb48f5438f8bbaf38f275e704e1541622d3db462bdfd027afcee6efe/Chakravarty%20et%20al.%20-%202020%20-%20Native%20Custom%20Tokens%20in%20the%20Extended%20UTXO%20Model.pdf)
- [UTxO- vs account-based smart contract
blockchain programming paradigms](https://arxiv.org/pdf/2003.14271.pdf)
- [The Architecture of Decentralised Finance Platforms: A New Open Finance Paradigm](https://iohk.io/en/research/library/papers/the-architecture-of-decentralised-finance-platformsa-new-open-finance-paradigm/)
- [Smart Contract Derivatives](https://eprint.iacr.org/2020/138.pdf)
- [Marlowe: implementing and analysing financial contracts on blockchain](https://files.zotero.net/eyJleHBpcmVzIjoxNjI3NDc1OTE2LCJoYXNoIjoiM2IzNzJmMzE2Yjc1M2Q0NzU4YzMxZDVhYWE0MTg3OGMiLCJjb250ZW50VHlwZSI6ImFwcGxpY2F0aW9uXC9wZGYiLCJjaGFyc2V0IjoiIiwiZmlsZW5hbWUiOiJTZWlqYXMgZXQgYWwuIC0gTWFybG93ZSBpbXBsZW1lbnRpbmcgYW5kIGFuYWx5c2luZyBmaW5hbmNpYWwgY29udC5wZGYifQ%3D%3D/316d015db70acade1a95d980cd88d9412242accbe9dffa62f13cd2673e067b6e/Seijas%20et%20al.%20-%20Marlowe%20implementing%20and%20analysing%20financial%20cont.pdf)
- [Functional Blockchain Contracts](https://files.zotero.net/eyJleHBpcmVzIjoxNjI3NDc1OTU2LCJoYXNoIjoiZWQ1YTU5MGZmYWFhYzQ2Y2VjNTFmNjlmZTQ0NjgzMjIiLCJjb250ZW50VHlwZSI6ImFwcGxpY2F0aW9uXC9wZGYiLCJjaGFyc2V0IjoiIiwiZmlsZW5hbWUiOiJDaGFrcmF2YXJ0eSBldCBhbC4gLSAyMDE5IC0gRnVuY3Rpb25hbCBCbG9ja2NoYWluIENvbnRyYWN0cy5wZGYifQ%3D%3D/46c4f06af19f34d11d25d4cce757d9034bb5dc8b5149252584901ad7f26215d9/Chakravarty%20et%20al.%20-%202019%20-%20Functional%20Blockchain%20Contracts.pdf)
- [Marlowe: financial contracts on blockchain](https://files.zotero.net/eyJleHBpcmVzIjoxNjI3NDc1OTk0LCJoYXNoIjoiM2JlM2Q4OWI5ZTE1YTIwMmYwZDY0Zjk2NmZmYTgzNTgiLCJjb250ZW50VHlwZSI6ImFwcGxpY2F0aW9uXC9wZGYiLCJjaGFyc2V0IjoiIiwiZmlsZW5hbWUiOiJTZWlqYXMgYW5kIFRob21wc29uIC0gTWFybG93ZSBmaW5hbmNpYWwgY29udHJhY3RzIG9uIGJsb2NrY2hhaW4ucGRmIn0%3D/62b91acae6e45230591758711825545e8d6756d7630e2c2e18b7e9dcf6c5bc6c/Seijas%20and%20Thompson%20-%20Marlowe%20financial%20contracts%20on%20blockchain.pdf)
- [Plutus: what you need to know, by Lars](https://iohk.io/en/blog/posts/2021/04/13/plutus-what-you-need-to-know/)


### Linkedin : [Kappos Angelos](https://www.linkedin.com/in/angelos-dionysios-kappos-4b668140/)

### **Twitter accounts:**
- [angelokappos](https://twitter.com/angelokappos)
- [sapiopool](https://twitter.com/sapiopool)

**Who is SapioPool Cardano Community** : 
- [Sapiopool Website](https://sapiopool.com/)

**Participate/collaborate in Sapiopool Greek Community *Discord* channel here** :
- [Discord Sapiopool](https://discord.com/invite/HRK9gGE9ax)


