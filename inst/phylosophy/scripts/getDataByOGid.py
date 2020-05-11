# -*- coding: utf-8 -*-

#######################################################################
# Copyright (C) 2020 Hannah Mülbaier & Vinh Tran
#
#  This file is part of phylosophy.
#
#  phylosophy is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  phylosophy is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with phylosophy.  If not, see <http://www.gnu.org/licenses/>.
#
#  Contact: hannah.muelbaier@gmail.com or tran@bio.uni-frankfurt.de
#
#######################################################################

import sys
import os
import argparse
from pathlib import Path
import time
from Bio import SeqIO
import multiprocessing as mp
import dccFn

def getOGprot(dataPath, omaGroupId):
    fileGroups = dccFn.openFileToRead(dataPath + "/oma-groups.txt")
    allGroups = fileGroups.readlines()
    fileGroups.close()
    groupLine = allGroups[int(omaGroupId) + 2].strip().split("\t")
    proteinIds = groupLine[2:]
    return(proteinIds)

def main():
    version = "1.0.0"
    parser = argparse.ArgumentParser(description="You are running getGenomes version " + str(version) + ".")
    required = parser.add_argument_group('required arguments')
    optional = parser.add_argument_group('additional arguments')
    required.add_argument('-g', '--OG', help='Input OMA group ID', action='store', default='', required=True)
    required.add_argument('-n', '--name', help='List of OMA species abbr. names', action='store', default='', required=True)
    required.add_argument('-i', '--id', help='List of corresponding taxonomy IDs to OMA species', action='store', default='', required=True)
    required.add_argument('-d', '--dataPath', help='Path to OMA Browser data', action='store', default='', required=True)
    required.add_argument('-o', '--outPath', help='Path to output directory', action='store', default='', required=True)
    optional.add_argument('-a', '--alignTool', help='Alignment tool (mafft|muscle). Default: mafft', action='store', default='mafft')
    args = parser.parse_args()

    dccFn.checkFileExist(args.dataPath)
    dccFn.checkFileExist(args.dataPath+"/oma-seqs-dic.fa")
    dataPath = str(Path(args.dataPath).resolve())
    omaGroupId = args.OG
    speciesList = str(args.name).split(",")
    speciesTaxId = str(args.id).split(",")
    outPath = str(Path(args.outPath).resolve())
    aligTool = args.alignTool.lower()
    if not aligTool == "mafft" or aligTool == "muscle":
        sys.exit("alignment tool must be either mafft or muscle")

    start = time.time()
    pool = mp.Pool(mp.cpu_count()-1)

    ### create output folders
    print("Creating output folders...")
    Path(outPath + "/genome_dir").mkdir(parents = True, exist_ok = True)
    Path(outPath + "/blast_dir").mkdir(parents = True, exist_ok = True)
    Path(outPath + "/core_orthologs").mkdir(parents = True, exist_ok = True)
    Path(outPath + "/core_orthologs/" + omaGroupId).mkdir(parents = True, exist_ok = True)
    Path(outPath + "/core_orthologs/" + omaGroupId + "/hmm_dir").mkdir(parents = True, exist_ok = True)

    ### Get genesets
    print("Getting %s gene sets..." % (len(speciesList)))
    dccFn.getGeneset(dataPath, speciesList, speciesTaxId, outPath)

    # read fasta file to dictionary
    fasta = {}
    blastJobs = []
    for i in range(0,len(speciesList)):
        fileName = dccFn.makeOneSeqSpeciesName(speciesList[i], speciesTaxId[i])
        specFile = outPath+"/genome_dir/"+fileName+"/"+fileName+".fa"
        fasta[speciesList[i]] = SeqIO.to_dict(SeqIO.parse(open(specFile),'fasta'))

        blastDbFile = "%s/blast_dir/%s/%s.phr" % (outPath, fileName, fileName)
        if not Path(blastDbFile).exists():
            blastJobs.append([fileName, specFile, outPath])

    ### create blastDBs
    print("Creating BLAST databases for %s taxa..." % len(blastJobs))
    msa = pool.map(dccFn.runBlast, blastJobs)

    ### get OG fasta
    print("Getting protein sequences for OG id %s..." % omaGroupId)
    proteinIds = getOGprot(dataPath, omaGroupId)
    ogFasta = outPath + "/core_orthologs/" + omaGroupId + "/" + omaGroupId
    flag = 1
    if Path(ogFasta + ".fa").exists():
        tmp = SeqIO.to_dict(SeqIO.parse(open(ogFasta + ".fa"),'fasta'))
        if len(tmp) == len(proteinIds):
            flag = 0
    if flag == 1:
        with open(ogFasta + ".fa", "w") as myfile:
            for protId in proteinIds:
                spec = protId[0:5]
                try:
                    seq = str(fasta[spec][protId].seq)
                    myfile.write(">" + omaGroupId + "|" + spec + "|" + protId + "\n" + seq + "\n")
                except:
                    print("%s not found in %s gene set" % (protId, spec))

    ### do MSA
    try:
        dccFn.runMsa([ogFasta, aligTool, omaGroupId])
    except:
        sys.exit("%s not found or %s not works correctly!" % (ogFasta+".fa", aligTool))

    ### do pHMM
    try:
        hmmFile = "%s/core_orthologs/%s/hmm_dir/%s.hmm" % (outPath, omaGroupId, omaGroupId)
        flag = 0
        try:
            if os.path.getsize(hmmFile) == 0:
                flag = 1
        except OSError as e:
                flag = 1
        if flag == 1:
            dccFn.runHmm([hmmFile, ogFasta, omaGroupId])
    except:
        sys.exit("hmmbuild not works correctly for %s!" % (ogFasta+".fa"))

    ende = time.time()
    print("Finished in " + '{:5.3f}s'.format(ende-start))
    print("Output can be found in %s" % outPath)

if __name__ == '__main__':
    main()