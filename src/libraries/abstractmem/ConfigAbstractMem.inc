{
  This file is part of AbstractMem framework

  Copyright (C) 2020-2021 Albert Molina - bpascalblockchain@gmail.com
  
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

{.$define ABSTRACTMEM_CIRCULAR_SEARCH_PROTECTION}
// define this to prevent circular search on tree nodes

{$if (defined(ABSTRACTMEM_TESTING_MODE)) or (defined(ABSTRACTMEM_USE_TLOG))}{$define ABSTRACTMEM_ENABLE_STATS}{$endif}

{ 
  HISTORY
  
  Version 0.1 - January-April 2020
  - First implementation for use in PascalCoin project as a File/Mem cached struct to store SafeBox
  - Creation of TAbstractMem, TAVLAbstractTree and TCacheMem for use in TFileMem
  - Initial tests

  Version 1.0 - May 2020
  - Integration with PascalCoin project and final tests

  Version 1.1 - Nov 2020
  - Improve CacheMem performance using predefined size blocks by default on Cache, this increases speed (on PascalCoin tested) up to 4x vs previous version
  - Added tests
  - Fixed bug on CacheMem when replacing initial position of buffer

  Version 1.2 - Jan 2021
  - Added TAbstractBTree - Standard B-Tree implementation for use on AbstractMem Library - Multithread protected
  - Added TAbstractMemBTreeData<TData> that implements a generic <TData> implementation for TAbstractBTree on TAbstractMem
  - Added ABSTRACTMEM_CIRCULAR_SEARCH_PROTECTION compiler directive to prevent circular structures on Tree nodes
  
  Version 1.3 - Jul 2021
  - Added TAbstractMemZoneInfo that allows to Analyze TAbstractMem using CheckConsistency and returns all blocks information
  - Added TAVLCacheStats that allows to obtain stats from any TAVLCache<T> object
  
  Version 1.4 - Oct 2021
  - Fixed but to allow files up to 4 Gb. Limitation is due to internal 4 bytes positioning. 4 bytes = 4 Gb maximum size

  Version 1.5 - Nov 2021
  - Allows 32 or 64 bits file (More than 4Gb limitation)
  - Allows select Max allowed block size by setting units size:
    - 32 bits: Fixed 1 unit = 4 bytes: Max size = ((2^24)*4) = 67108864 bytes = 64Mb
    - 64 bits: Customizable: 1 unit from 4..256 bytes (multiple of 4): Max size = from ((2^32)*4) 16Gb ... ((2^32)*256) = 1099511627776 = 1Tb

}

const
  CT_ABSTRACTMEM_VERSION = 1.5; // Each revision should increase this version...


