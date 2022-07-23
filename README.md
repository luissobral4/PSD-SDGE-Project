### Autenticaçao
* login
* register

### Informacoes
* Event <string>
* logout

### Coletor -> Agregador
* login -> "login|User|Type"
* logout -> "logout|User"
* event -> "event|User|Message(event)
* inactive -> "inactive|User

### Portas
1. Coletor - Porta | Agregador - Porta+1
2. Coletor - Porta + 100 | Agregador - Porta + 100 + 1
3. Aggregador - Porta PULL | Porta+1 PUSH | Porta+2 REP
4. Client - Porta SUB | Porta+1 REQ

### Notificações
1. notificar quando deixar de haver dispositivos online de um dado **tipo** na zona
    * Sub: 'offline'-**TYPE**
    * Pub: 'offline'-**TYPE**
    * no agregador (quando é um logout), fazer a verificação se é o último dispositivo de um dado tipo
2. atingido record de número de dispositivos online de um dado tipo na zona (com informação do seu valor); um cliente poderá estar interessado em saber se foi atingido algum record, para algum tipo de dispositivo (e não um tipo em particular);
    * Sub: 'record'-**TYPE** | se for para todos: 'record'
    * Pub: 'record'-**TYPE**
3. a percentagem de dispositivos online na zona face ao total online subiu, tendo ficado em mais de X% dos dispositivos online, para X ∈ {10, 20, . . . , 90}.
    * Sub: 'percentUp-'**PERCENT**
    * Pub: 'percentUp-'**PERCENT**
4. a percentagem de dispositivos online na zona face ao total online desceu, tendo ficado em menos de X% dos dispositivos online, para X ∈ {10, 20, . . . , 90}.
    * Sub: 'percentDown-'**PERCENT**
    * Pub: 'percentDown-'**PERCENT**


### Separação
**Separação:**
version, source, state/msg , user~Device
Device -> user:type:online:active:Event
Event -> event%numeroOcorrencia ! Event
Event -> 

**exemplo:**
5,1201,state,user1~user1:car:true:false:brake%2!turn-left%1!hand-brake-ok%4!accelerate%2!hand-brake-off%3!turn-right%1


### O QUE FALTA FAZER
* corrigir notificações up e down
* melhorar menus caso haja tempo
* sockets erlang
* comentários
* relatório