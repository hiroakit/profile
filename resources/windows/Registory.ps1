# レジストリ変更スクリプト

# ItemPropertyが存在する場合にtrue、それ以外にfalseを返す
#
# $Path
#   レジストリのパスを指定します
#   例) HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\AppModelUnlock
# $Name 
#   レジストリの項目名を指定します
#   例) AllowAllTrustedApps
function HasItemProperty( $Path, $Name ){
    # パスがレジストリに存在するか確認する
    if(!(Test-Path $Path)){
        Write-Host ("$Path not found. Please check path.")
        return $FALSE
    }

    # ItemPropertyの存在を確認する
    $item = Get-ItemProperty $Path -Name $Name -ErrorAction SilentlyContinue
    if($null -eq $item){
        return $FALSE
    }
    return $TRUE
}

# 開発者モードを有効にする
#
# HKLM => HKLM HKEY_LOCAL_MACHINE
# HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\AppModelUnlock
#
# AllowAllTrustedApps:1, AllowDevelopmentWithoutDevLicense:1 で開発者モードが有効になる
function TurnOnDeveloperMode () {
  # レジストリのパス
  $DeveloperModePath = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\AppModelUnlock"

  # AllowAllTrustedApps
  if (HasItemProperty $DeveloperModePath AllowAllTrustedApps) {
    Set-ItemProperty -Path $DeveloperModePath -Name AllowAllTrustedApps -Value 1 
    Write-Host ("Update AllowAllTrustedApps value.")
  } else {
    New-ItemProperty -Path $DeveloperModePath -Name AllowAllTrustedApps -PropertyType DWord -Value 1
    Write-Host ("Add AllowAllTrustedApps.")
  }

  # AllowDevelopmentWithoutDevLicense
  if (HasItemProperty $DeveloperModePath AllowDevelopmentWithoutDevLicense) {
    Set-ItemProperty -Path $DeveloperModePath -Name AllowDevelopmentWithoutDevLicense -Value 1 
    Write-Host ("Update AllowDevelopmentWithoutDevLicense value.")
  } else {
    New-ItemProperty -Path $DeveloperModePath -Name AllowDevelopmentWithoutDevLicense -PropertyType DWord -Value 1
    Write-Host ("Add AllowDevelopmentWithoutDevLicense.")
  }
}

TurnOnDeveloperMode
