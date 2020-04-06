# Quatre en ratlla

Aquesta pàgina descriu **Quatre en ratlla**, la pràctica de Haskell que
substitueix l'examen parcial de LP durant l'Era del Confinament.


## Presentació

Es demana que feu un programa en Haskell que permeti jugar a un humà al
quatre en ratlla contra diferentes estratègies de l'ordinador.

![](4ratlla.png)

El joc del quatre en ratlla es juga en un tauler `n` × `m`, on `n` i `m` es
defineixen al iniciar la partida. El cas més habitual és el 6×7, és a dir, 6
files i 7 columnes. Cada jugador té `n`×`m` fitxes d'un mateix color. Per
exemple, unes son vermelles i les altres grogues. Els jugadors introdueixen
alternativament una fitxa del seu color en una columna. La fitxa cau fins
dipositar-se al d'amunt de l'última fitxa que s'ha introduït en la mateixa
columna. Un jugador guanya quan després d'introduir una fitxa hi ha 4 fitxes
consecutives del seu color formant una línia horitzontal, vertical o diagonal.
La partida acaba quan un jugador guanya o s'omple el tauler.


## Estratègies

Una estratègia és un algorisme que segueix l'ordinador per intentar guanyar el
joc. Més concretament, en aquesta pràctica, una estratègia és una funció que rep
el tauler i el jugador a qui toca moure i retorna la columna on posar la seva
fitxa. Heu de fer que les funcions del joc siguin parametritzades amb
l'estratègia escollida.

Cal que programeu tres estratègies:

- estratègia `random`: cada tirada de l'ordinador és una columna a l'atzar.

- estratègia `greedy`: cada tirada de l'ordinador és a la columna que li
permet posar en ratlla el nombre més alt de fitxes pròpies i que evita
(si pot) que el contrari faci 4-en-ratlla a la jugada següent. En cas
d'empat, tria arbitràriament.

- estratègia `smart`: trieu vosaltres una estratègia el més astuta possible.
No hauria de ser massa lenta (un parell de segons màxim per jugada, diguem).


## Nombres aleatoris en Haskell

El programa següent mostra com treballar senzillament amb nombres aleatoris en
Haskell:

```haskell
import System.Random

randInt :: Int -> Int -> IO Int
-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).

randInt low high = do
    random <- randomIO :: IO Int
    let result = low + random `mod` (high - low + 1)
    return result

main :: IO ()
-- main program that throws two dice.

main = do
    r1 <- randInt 1 6
    r2 <- randInt 1 6
    print (r1, r2)
```

Fixeu-vos que els nombres aleatoris s'obtenen dins de la mònada IO,
utilitzant l'acció
`randomIO :: IO a`
on `a` pot ser `Bool`, `Char`, `Double`, `Float`, `Int`, `Integer`...

Per poder importar `System.Random`, segurament haureu d'instal·lar
abans el paquet `random`. En Mac:

```bash
> brew install cabal-install
> cabal update
> cabal install --lib random
```

En Ubuntu:

```bash
> sudo apt install cabal-install
> cabal update
> cabal install random
```

Sembla que segons els sistema i la versió cal jugar una mica
amb el `--lib`.


## Lliurament

Només heu de lliurar un fitxer ZIP que, al descomprimir-se,
generi els fitxers següents:

- `joc.hs`: el codi del vostre programa,

- `README.md`: la documentació de la vostra pràctica,

- `*.png` si cal adjuntar imatges a la documentació.

Res més. Sense directoris ni subdirectoris.

El codi s'ha de poder compilar i generar un executable amb la comanda
`ghc joc.hs`. Totes les funcions i
definicions de tipus han d'estar
documentades en el propi codi amb comentaris adients.

El projecte ha de contenir un fitxer `README.md` que el documenti
adequadament. Vegeu, per
exemple, [aquest document](https://gist.github.com/PurpleBooth/109311bb0361f32d87a2). Si us calen
imatges al `README.md`, deseu-les com a fitxers PNG.

El lliurament s'ha de fer a través del Racó, abans del **diumenge 26 d'abril a les
23:59**.


## Observacions

- L'enunciat deixa obertes moltes qüestions intencionadament (qui comença, estratègia
astuta, entrada/sortida, ...). Sou els responsables de prendre les vostres
pròpies decisions i deixar-les reflectides adientment al codi i a la
documentació. Ara bé, fixeu-vos que és imperatiu que les funcions del joc siguin
funcions d'ordre superior que reben l'estratègia com a paràmetre.

- Intenteu desacoplar tant som sigui possible l'entrada/sortida del càlcul.

- Es valorarà la qualitat i originalitat de la vostra  estratègia `smart`.

- Es valorarà l'ús de funcions d'ordre superior i de les construccions pròpies a Haskell.

- S'utilitzaran programes detectors de plagi per detectar possibles còpies.

- Per evitar problemes de còpies indesitjades, no pengeu el vostre projecte en repositoris
públics. Si us cal un repositori GIT, useu [GITLAB
FIB](https://gitlab.fib.upc.edu/users/sign_in).
