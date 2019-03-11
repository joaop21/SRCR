%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Conhecimento para caracterizar um universo de discurso na área da prestação de cuidados de saúde.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic utente/4.
:- dynamic servico/4.
:- dynamic consulta/4.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: IdUt,Nome,Idade,Cidade-> {V,F}

utente(1,joao,31,guimaraes).
utente(2,manuel,57,viana).
utente(3,armando,26,porto).
utente(4,ricardo,23,famalicao).
utente(5,maria,40,braga).
utente(6,miguel,26,guimaraes).
utente(7,ana,14,braga).
utente(8,andre,26,amares).
utente(9,henrique,14,fafe).
utente(10,diogo,14,braga).


% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido

+utente( IU,_,_,_ ) :: (solucoes( IU,(utente( IU,_,_,_ )),S ),
                  comprimento( S,L ),
				          L == 1).

% Referencial ????????????????
% Invariante Referencial: a idade de cada utente tem de ser inteira e
%             estar no intervalo [0,120]

+utente( _,_,I,_ ) :: (integer(I),
                      I >= 0,
                      I =< 120).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado serviço: IdServ,Descrição,Instituição,Cidade -> {V,F}

servico(1,cardiologia,hospitaldaluz,guimaraes).
servico(2,pediatria,hospitalbraga,braga).
servico(3,cirurgia,hospitalbraga,braga).
servico(4,neurologia,hsj,porto).
servico(5,ginecologia,hospitalbraga,braga).
servico(6,psiquiatria,hsog,guimaraes).
servico(7,oftamologia,hsog,guimaraes).


% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido

+servico( IS,_,_,_ ) :: (solucoes( IS,(servico( IS,_,_,_ )),S ),
                  comprimento( S,L ),
				          L == 1).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado consulta: Data,IdUt,IdServ,Custo-> {V,F}

consulta(01-02-2019, 1, 6, 25).
consulta(13-02-2019, 3, 4, 30).
consulta(13-02-2019, 5, 5, 35).
consulta(14-02-2019, 2, 7, 9).
consulta(20-02-2019, 7, 2, 20).
consulta(23-02-2019, 7, 8, 5).
consulta(23-02-2019, 5, 5, 24).
consulta(25-02-2019, 6, 7, 40).
consulta(29-02-2019, 7, 2, 65).
consulta(04-03-2019, 9, 2, 95).
consulta(07-03-2019, 1, 1, 10).


%--------------------------PONTO 1--------------------------%

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Registar Utente : IdUt,Nome,Idade,Cidade-> {V,F}

  registarU( IU,N,I,C ) :-
    evolucao(utente( IU,N,I,C )).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Registar Serviço : IdServ,Descrição,Instituição,Cidade -> {V,F}

  registarServ( IS,D,I,C ) :-
    evolucao(servico( IS,D,I,C )).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Registar Consulta : Data,IdUt,IdServ,Custo -> {V,F}

  registarConsulta( DA,IU,IS,C ) :-
    evolucao(consulta( DA,IU,IS,C )).



%--------------------------PONTO 2--------------------------%

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Remover Utente : IdUt,Nome,Idade,Cidade-> {V,F}

  removerU( IU,N,I,C ) :-
    regressao(utente( IU,N,I,C )).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Remover Serviço : IdServ,Descrição,Instituição,Cidade -> {V,F}

  removerServ(IS,D,I,C) :-
    regressao(servico( IS,D,I,C )).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Remover Consulta : Data,IdUt,IdServ,Custo -> {V,F}

  removerConsulta( DA,IU,IS,C ) :-
    regressao(consulta( DA,IU,IS,C )).



%--------------------------PONTO 3--------------------------%

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado instituicoes: Resultado -> {V,F}

instituicoes(R) :-
    solucoes(INST, servico(IDC,DESC,INST,CD), LR),
    removeReps(LR,R).



%--------------------------PONTO 4--------------------------%

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesPNome: Nome, Resultado -> {V,F}

utentesPNome(Nome,R) :-
    solucoes( (IU,Nome,I,C), utente(IU,Nome,I,C), R).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesPIdade: Idade, Resultado -> {V,F}

utentesPIdade(Idade,R) :-
    solucoes( (IU,N,Idade,C), utente(IU,N,Idade,C), R).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado utentesPCidade: Cidade, Resultado -> {V,F}

utentesPCidade(Cidade,R) :-
    solucoes( (IU,N,I,Cidade), utente(IU,N,I,Cidade), R).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado servicosPDesc: Descrição, Resultado -> {V,F}

servicosPDesc(Descricao,R) :-
    solucoes( (IS,Descricao,I,C), servico( IS,Descricao,I,C ), R).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado consultasPData: Data, Resultado -> {V,F}

consultasPData(Data,R) :-
    solucoes( (Data,IU,IS,C), consulta( Data,IU,IS,C ), R).



%--------------------------PONTO 5--------------------------%

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado servicosPInst: Instituição, Resultado -> {V,F}

servicosPInst(Instituicao,R) :-
    solucoes( (IS,D,Instituicao,C), servico( IS,D,Instituicao,C ), R).

% ----------------------------------------------------------------------------------------------------
% Extensao do predicado servicosPCidade: Cidade, Resultado -> {V,F}

servicosPCidade(Cidade,R) :-
    solucoes( (IS,D,I,Cidade), servico( IS,D,I,Cidade ), R).



%--------------------------PREDICADOS AUXILIARES--------------------------%

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento

evolucao( Termo ) :-
  solucoes( Invariante,+Termo::Invariante,Lista ),
  insercao( Termo ),
  teste( Lista ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a regressão do conhecimento

regressao(Termo) :-
	Termo,
	solucoes( Invariante,-Termo::Invariante,Lista ),
	remover( Termo ),
  teste( Lista ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite encontrar as provas

solucoes(F,Q,S) :- Q, assert(tmp(F)), fail.
solucoes(F,Q,S) :- construir(S,[]).

construir(S1,S2) :- retract(tmp(X)), !, construir(S1, [X|S2]).
construir(S,S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a inserção do conhecimento
insercao( Termo ) :-
  assert( Termo ).
insercao( Termo ) :-
  retract( Termo ), !, fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a remoção do conhecimento
remover(Termo) :-
  retract(Termo).
remover(Termo) :-
  assert(Termo), !, fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que realiza o teste do conhecimento
teste([]).
teste([R|LR]) :-
  R,
  teste(LR).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado comprimento: L,N -> {V,F}

comprimento([], 0).
comprimento([H|T], N) :-
  comprimento(T,S),
  N is S+1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado removeReps: L,R -> {V,F}

removeReps([], []).
removeReps([H|T], R) :-
    member(H,T),
	removeReps(T, R).
removeReps([H|T],[H|R]) :-
    nao(member(H,T)),
    removeReps(T,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado nao: T -> {V,F}

nao(T) :-
    T, !, fail.
nao(T).
