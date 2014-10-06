// main11.cc is a part of the PYTHIA event generator.
// Copyright (C) 2013 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This is a simple test program. 
// It illustrates how Les Houches Event File input can be used in Pythia8.
// It uses the ttsample.lhe input file, the latter only with 100 events.

#include "Pythia8/Pythia.h"
#include "Pythia8/Pythia8ToHepMC.h"

#include "HepMC/GenEvent.h"
#include "HepMC/IO_GenEvent.h"
// Following line to be used with HepMC 2.04 onwards.
#include "HepMC/Units.h"

using namespace Pythia8; 
int main() {

  // Generator. We here stick with default values, but changes
  // could be inserted with readString or readFile.          
  Pythia pythia;                            

  // Initialize Les Houches Event File run. List initialization information.
  pythia.readString("Beams:frameType = 4"); 
  pythia.readString("Beams:LHEF = test.lhe"); 
  pythia.init();      


  HepMC::Pythia8ToHepMC ToHepMC;
  // Specify file where HepMC events will be stored.
  HepMC::IO_GenEvent ascii_io("mytest.hepmc", std::ios::out);
  // Switch off warnings for parton-level events.
  ToHepMC.set_print_inconsistency(false);
  ToHepMC.set_free_parton_warnings(false);
  // Do not store cross section information, as this will be done manually.
  ToHepMC.set_store_pdf(false);
  ToHepMC.set_store_proc(false);
  ToHepMC.set_store_xsec(false);


  // Book histogram.
  Hist nCharged("charged particle multiplicity",100,-0.5,399.5); 

  // Allow for possibility of a few faulty events.
  int nAbort = 10;
  int iAbort = 0;

  // Begin event loop; generate until none left in input file.     
  for (int iEvent = 0; ; ++iEvent) {

    // Generate events, and check whether generation failed.
    if (!pythia.next()) {

      // If failure because reached end of file then exit event loop.
      if (pythia.info.atEndOfFile()) break; 

      // First few failures write off as "acceptable" errors, then quit.
      if (++iAbort < nAbort) continue;
      break;
    }

    // Sum up final charged multiplicity and fill in histogram.
    int nChg = 0;                 
    for (int i = 0; i < pythia.event.size(); ++i) 
        if (pythia.event[i].isFinal() && pythia.event[i].isCharged()) 
            ++nChg;
    nCharged.fill(nChg);               

      // Construct new empty HepMC event.
      HepMC::GenEvent* hepmcevt = new HepMC::GenEvent();
      // Get correct cross section from previous estimate.
      // Set event weight
      hepmcevt->weights().push_back(1.0);
      // Fill HepMC event.
      ToHepMC.fill_next_event( pythia, hepmcevt );
      // Report cross section to hepmc
      //HepMC::GenCrossSection xsec;
      // xsec.set_cross_section( sigmaTotal*1e9, pythia.info.sigmaErr()*1e9 );
      // hepmcevt->set_cross_section( xsec );
      // Write the HepMC event to file. Done with it.
      ascii_io << hepmcevt;
      delete hepmcevt;


  // End of event loop.        
  }                                           

  // Give statistics. Print histogram.
  pythia.stat();
  // cout << nCharged;  

  // Done.                           
  return 0;
}
