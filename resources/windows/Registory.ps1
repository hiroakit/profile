# レジストリ変更スクリプト
#
# HKCR: => HKEY_CLASSES_ROOT
# HKCU: => HKEY_CURRENT_USER
# HKLM: => HKEY_LOCAL_MACHINE

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
# 該当パス:
#   HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\AppModelUnlock
# 
# ノート:
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

# 開発者モードを有効にする
TurnOnDeveloperMode

# Thumbs.dbの生成をやめる
# Set-ItemProperty "HKCU:\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer" -Name "NoThumbnailCache" -Value "1";

# エクスプローラーを開く際の初期表示画面 (0:クイックアクセス 1: PC)
Set-ItemProperty "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced" -Name LaunchTo -Value 1

# エクスプローラーのクイックアクセスに最近使ったファイルを表示させない
Set-ItemProperty "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer" -Name ShowRecent -Value 0

# エクスプローラーのクイックアクセスによく使うフォルダを表示させない
Set-ItemProperty "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer" -Name ShowFrequent -Value 0

# ファイルの拡張子を表示する
# その後、Stop-ProcessでExplorerの停止(すぐに再起動する)
Set-ItemProperty "HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced" -Name "HideFileExt" -Value "0";
Write-Host ("Rebooting Explorer.")
Stop-Process -Name Explorer -Force;
