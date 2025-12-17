{***************************************************************************}
{                                                                           }
{           Dext Framework                                                  }
{                                                                           }
{           Global Configuration & Compiler Directives                      }
{                                                                           }
{***************************************************************************}

{$IFNDEF DEXT_INC}
{$DEFINE DEXT_INC}

//---------------------------------------------------------------------------
// Compiler Features
//---------------------------------------------------------------------------

// System.Hash unit (Introduced in Delphi XE8 / CompilerVersion 29.0)
{$IF CompilerVersion >= 29.0}
  {$DEFINE DEXT_HAS_SYSTEM_HASH}
{$IFEND}

//---------------------------------------------------------------------------
// User Overrides
//---------------------------------------------------------------------------

// Define DEXT_FORCE_INDY_HASH in your project options or here
// to force usage of Indy for hashing even on newer Delphi versions.
{$IFDEF DEXT_FORCE_INDY_HASH}
  {$UNDEF DEXT_HAS_SYSTEM_HASH}
{$ENDIF}

{$ENDIF}
