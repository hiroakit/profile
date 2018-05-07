const puppeteer = require('puppeteer');

(async () => {
  const browser = await puppeteer.launch();
  const page = await browser.newPage();
  const downloadPath = './Download';

  const toolName = 'Command_Line_Tools_macOS_10.13_for_Xcode_9.3';
  const fileUrl = 'https://download.developer.apple.com/Developer_Tools/' + toolName + '/' + toolName + '.dmg';

  await page._client.send(
    'Page.setDownloadBehavior',
    {behavior : 'allow', downloadPath: downloadPath}
  );

  await page.goto('https://developer.apple.com/download/more/');
  await page.type('#accountname', 'hiroakiendoh@outlook.jp');
  await page.type('#accountpassword', 'wULCKd6KfkM#bY7ernmzhNe74F*E=qw#');
  await page.click('#submitButton2');
  await page.waitFor(5000); // millseconds
  
  
  await page.goto(fileUrl, {waitUntil: 'networkidle2'}).catch((err) => {
    browser.close();
  });
  console.log('finished');
  browser.close();
})();
