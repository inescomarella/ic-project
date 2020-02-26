##################### 2020-02-32 ###### Inês Motta Comarella ##################

####### Shell script para padronizar as pastas do projeto de IC "Seleção de Hábitat e relações competitivas entre carnívoros em paisagem fragmentada" ########

#### ATENÇÃO! Não é possível rodar como um script, os comandos devem ser rodados no terminal, para isso digite bash no terminal, dê enter e siga o pass-a-passo

### Referência: https://www.earthdatascience.org/courses/intro-to-earth-data-science/open-reproducible-science/bash/bash-commands-to-manage-directories-files/ ###


##### Comandos -------------
## Print Current Working Directory (pwd)
pwd

## Change Current Working Directory (cd)
cd ..

## Create a New Directory (mkdir)
mkdir directory-name

## Print a List of Files and Subdirectories (ls)
ls

## Delete a File (rm)
rm file-name

## Delete a Directory (rm -r)
rm -r directory-name

## Copy a File (cp)
cp file-name directory-name

## Copy a Directory and Its Contents (cp -r)
cp -r directory-name-1 directory-name-2

## Create a New File Using a Single Command (touch)
touch file-name.txt
## Rename a file/directory (mv)
mv old-name new-name




##### Criando diretórios ---------------
# Crie os diretórios desejados no local escolhido

mkdir IC-project
cd IC-project

mkdir data
mkdir doc
mkdir figs
mkdir output
mkdir results
mkdir R


####### Utilize o Advanced R style coding para nomear os arquivos e variáveis ------------------
#### https://adv-r.hadley.nz/index.html

## n-file-name.R
# 0-download.R
# 1-parse.R
# 2-exploring.R

## object_name

git clone git@github.com:inescomarella/ic-project.git


