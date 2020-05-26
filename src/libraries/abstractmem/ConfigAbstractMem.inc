{
  This file is part of AbstractMem framework

  Copyright (C) 2020 Albert Molina - bpascalblockchain@gmail.com
  
  https://github.com/PascalCoinDev/  

  *** BEGIN LICENSE BLOCK *****

  The contents of this files are subject to the Mozilla Public License Version
  2.0 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Initial Developer of the Original Code is Albert Molina.

  ***** END LICENSE BLOCK *****
}

{.$define ABSTRACTMEM_TESTING_MODE}
// define this if you want some testing mode capabilities

{.$define ABSTRACTMEM_ENABLE_STATS}
// define this to activate some stats on objects usefull for testing

{$if (defined(ABSTRACTMEM_TESTING_MODE)) or (defined(ABSTRACTMEM_USE_TLOG))}{$define ABSTRACTMEM_ENABLE_STATS}{$endif}

{ 
  HISTORY
  
  Version 0.1 - January-April 2020
  - First implementation for use in PascalCoin project as a File/Mem cached struct to store SafeBox
  - Creation of TAbstractMem, TAVLAbstractTree and TCacheMem for use in TFileMem
  - Initial tests

  Version 1.0 - May 2020
  - Integration with PascalCoin project and final tests
  

}

const
  CT_ABSTRACTMEM_VERSION = 1.0; // Each revision should increase this version...
