{***************************************************************************}
{                                                                           }
{           Dext Framework                                                  }
{                                                                           }
{           Copyright (C) 2025 Cesar Romero & Dext Contributors             }
{                                                                           }
{           Licensed under the Apache License, Version 2.0 (the "License"); }
{           you may not use this file except in compliance with the License.}
{           You may obtain a copy of the License at                         }
{                                                                           }
{               http://www.apache.org/licenses/LICENSE-2.0                  }
{                                                                           }
{           Unless required by applicable law or agreed to in writing,      }
{           software distributed under the License is distributed on an     }
{           "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    }
{           either express or implied. See the License for the specific     }
{           language governing permissions and limitations under the        }
{           License.                                                        }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Author:  Cesar Romero                                                    }
{  Created: 2025-12-08                                                      }
{                                                                           }
{***************************************************************************}
unit Dext.RateLimiting.Policy;

interface

uses
  System.SysUtils,
  Dext.Web.Interfaces,
  Dext.RateLimiting.Core;

type
  /// <summary>
  ///   Fluent builder for rate limiting policies.
  /// </summary>
  /// <summary>
  ///   Fluent builder for rate limiting policies.
  /// </summary>
  TRateLimitPolicy = record
  private
    FConfig: TRateLimitConfig;
    FInitialized: Boolean;
    procedure EnsureInitialized;
  public
    /// <summary>
    ///   Implicit initialization check.
    /// </summary>
    procedure CheckInit; 

    /// <summary>
    ///   Creates a Fixed Window rate limiter.
    /// </summary>
    class function FixedWindow(APermitLimit, AWindowSeconds: Integer): TRateLimitPolicy; static;
    
    /// <summary>
    ///   Creates a Sliding Window rate limiter.
    /// </summary>
    class function SlidingWindow(APermitLimit, AWindowSeconds: Integer): TRateLimitPolicy; static;
    
    /// <summary>
    ///   Creates a Token Bucket rate limiter.
    /// </summary>
    class function TokenBucket(ATokenLimit, ARefillRate: Integer): TRateLimitPolicy; static;
    
    /// <summary>
    ///   Creates a Concurrency limiter.
    /// </summary>
    class function Concurrency(ALimit: Integer): TRateLimitPolicy; static;
    
    // =====================================================================
    // New API (without 'With' prefix)
    // =====================================================================
    
    // Partition strategies
    function PartitionByIp: TRateLimitPolicy;
    function PartitionByHeader(const AHeaderName: string): TRateLimitPolicy;
    function PartitionByRoute: TRateLimitPolicy;
    function PartitionKey(AResolver: TPartitionKeyResolver): TRateLimitPolicy;
    
    // Global limits
    function GlobalLimit(AConcurrencyLimit: Integer): TRateLimitPolicy;
    
    // Response configuration
    function RejectionMessage(const AMessage: string): TRateLimitPolicy;
    function RejectionStatusCode(AStatusCode: Integer): TRateLimitPolicy;

    // =====================================================================
    // Deprecated API (with 'With' prefix)
    // =====================================================================
    
    function WithPartitionByIp: TRateLimitPolicy; deprecated 'Use PartitionByIp instead';
    function WithPartitionByHeader(const AHeaderName: string): TRateLimitPolicy; deprecated 'Use PartitionByHeader instead';
    function WithPartitionByRoute: TRateLimitPolicy; deprecated 'Use PartitionByRoute instead';
    function WithPartitionKey(AResolver: TPartitionKeyResolver): TRateLimitPolicy; deprecated 'Use PartitionKey instead';
    function WithGlobalLimit(AConcurrencyLimit: Integer): TRateLimitPolicy; deprecated 'Use GlobalLimit instead';
    function WithRejectionMessage(const AMessage: string): TRateLimitPolicy; deprecated 'Use RejectionMessage instead';
    function WithRejectionStatusCode(AStatusCode: Integer): TRateLimitPolicy; deprecated 'Use RejectionStatusCode instead';

    /// <summary>
    ///   Gets the configuration.
    /// </summary>
    function Build: TRateLimitConfig;
    
    property Config: TRateLimitConfig read FConfig;
  end;

  /// <summary>
  ///   Alias for TRateLimitPolicy (preferred).
  /// </summary>
  RateLimitPolicy = TRateLimitPolicy;

implementation

{ TRateLimitPolicy }

procedure TRateLimitPolicy.EnsureInitialized;
begin
  if not FInitialized then
  begin
    FConfig := TRateLimitConfig.Create;
    FInitialized := True;
  end;
end;

procedure TRateLimitPolicy.CheckInit;
begin
  EnsureInitialized;
end;

class function TRateLimitPolicy.FixedWindow(APermitLimit, AWindowSeconds: Integer): TRateLimitPolicy;
begin
  Result.EnsureInitialized;
  Result.FConfig.LimiterType := rltFixedWindow;
  Result.FConfig.PermitLimit := APermitLimit;
  Result.FConfig.WindowSeconds := AWindowSeconds;
end;

class function TRateLimitPolicy.SlidingWindow(APermitLimit, AWindowSeconds: Integer): TRateLimitPolicy;
begin
  Result.EnsureInitialized;
  Result.FConfig.LimiterType := rltSlidingWindow;
  Result.FConfig.PermitLimit := APermitLimit;
  Result.FConfig.WindowSeconds := AWindowSeconds;
end;

class function TRateLimitPolicy.TokenBucket(ATokenLimit, ARefillRate: Integer): TRateLimitPolicy;
begin
 Result.EnsureInitialized;
  Result.FConfig.LimiterType := rltTokenBucket;
  Result.FConfig.TokenLimit := ATokenLimit;
  Result.FConfig.RefillRate := ARefillRate;
end;

class function TRateLimitPolicy.Concurrency(ALimit: Integer): TRateLimitPolicy;
begin
  Result.EnsureInitialized;
  Result.FConfig.LimiterType := rltConcurrency;
  Result.FConfig.ConcurrencyLimit := ALimit;
end;

function TRateLimitPolicy.PartitionByIp: TRateLimitPolicy;
begin
  EnsureInitialized;
  FConfig.PartitionStrategy := psIpAddress;
  Result := Self;
end;

function TRateLimitPolicy.PartitionByHeader(const AHeaderName: string): TRateLimitPolicy;
begin
  EnsureInitialized;
  FConfig.PartitionStrategy := psHeader;
  FConfig.PartitionHeader := AHeaderName;
  Result := Self;
end;

function TRateLimitPolicy.PartitionByRoute: TRateLimitPolicy;
begin
  EnsureInitialized;
  FConfig.PartitionStrategy := psRoute;
  Result := Self;
end;

function TRateLimitPolicy.PartitionKey(AResolver: TPartitionKeyResolver): TRateLimitPolicy;
begin
  EnsureInitialized;
  FConfig.PartitionStrategy := psCustom;
  FConfig.PartitionResolver := AResolver;
  Result := Self;
end;

function TRateLimitPolicy.GlobalLimit(AConcurrencyLimit: Integer): TRateLimitPolicy;
begin
  EnsureInitialized;
  FConfig.EnableGlobalLimit := True;
  FConfig.GlobalConcurrencyLimit := AConcurrencyLimit;
  Result := Self;
end;

function TRateLimitPolicy.RejectionMessage(const AMessage: string): TRateLimitPolicy;
begin
  EnsureInitialized;
  FConfig.RejectionMessage := AMessage;
  Result := Self;
end;

function TRateLimitPolicy.RejectionStatusCode(AStatusCode: Integer): TRateLimitPolicy;
begin
  EnsureInitialized;
  FConfig.RejectionStatusCode := AStatusCode;
  Result := Self;
end;

// Deprecated

function TRateLimitPolicy.WithPartitionByIp: TRateLimitPolicy;
begin
  Result := PartitionByIp;
end;

function TRateLimitPolicy.WithPartitionByHeader(const AHeaderName: string): TRateLimitPolicy;
begin
  Result := PartitionByHeader(AHeaderName);
end;

function TRateLimitPolicy.WithPartitionByRoute: TRateLimitPolicy;
begin
  Result := PartitionByRoute;
end;

function TRateLimitPolicy.WithPartitionKey(AResolver: TPartitionKeyResolver): TRateLimitPolicy;
begin
  Result := PartitionKey(AResolver);
end;

function TRateLimitPolicy.WithGlobalLimit(AConcurrencyLimit: Integer): TRateLimitPolicy;
begin
  Result := GlobalLimit(AConcurrencyLimit);
end;

function TRateLimitPolicy.WithRejectionMessage(const AMessage: string): TRateLimitPolicy;
begin
  Result := RejectionMessage(AMessage);
end;

function TRateLimitPolicy.WithRejectionStatusCode(AStatusCode: Integer): TRateLimitPolicy;
begin
  Result := RejectionStatusCode(AStatusCode);
end;

function TRateLimitPolicy.Build: TRateLimitConfig;
begin
  EnsureInitialized;
  Result := FConfig;
end;

end.

