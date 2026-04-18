const {
  Document, Packer, Paragraph, TextRun, AlignmentType,
  Header, Footer, TabStopType, TabStopPosition, BorderStyle,
  PageNumberElement
} = require('docx');
const fs = require('fs');

const NAVY  = "1B3A6B";
const GOLD  = "C8960C";
const LGRAY = "F2F4F7";
const MGRAY = "7F8C9A";
const BLACK = "1A1A1A";

const doc = new Document({
  styles: {
    default: {
      document: { run: { font: "Calibri", size: 22, color: BLACK } }
    },
    paragraphStyles: [
      { id: "Title", name: "Title", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { font: "Calibri Light", size: 52, bold: true, color: NAVY },
        paragraph: { alignment: AlignmentType.CENTER, spacing: { before: 480, after: 120 } }},
      { id: "Subtitle", name: "Subtitle", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { font: "Calibri Light", size: 26, color: MGRAY, italics: true },
        paragraph: { alignment: AlignmentType.CENTER, spacing: { before: 0, after: 480 } }},
      { id: "Heading1", name: "Heading 1", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { font: "Calibri Light", size: 34, bold: true, color: NAVY },
        paragraph: { spacing: { before: 480, after: 120 }, outlineLevel: 0,
          border: { bottom: { style: BorderStyle.SINGLE, size: 6, color: GOLD, space: 6 } }}},
      { id: "Heading2", name: "Heading 2", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { font: "Calibri Light", size: 26, bold: true, color: NAVY },
        paragraph: { spacing: { before: 360, after: 80 }, outlineLevel: 1 }},
      { id: "Heading3", name: "Heading 3", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { font: "Calibri", size: 22, bold: true, color: MGRAY },
        paragraph: { spacing: { before: 240, after: 60 }, outlineLevel: 2 }},
      { id: "Normal", name: "Normal",
        run: { font: "Calibri", size: 22, color: BLACK },
        paragraph: { spacing: { line: 320, after: 120 }, alignment: AlignmentType.JUSTIFIED }},
      { id: "Quote", name: "Quote", basedOn: "Normal",
        run: { font: "Calibri", size: 22, color: NAVY, italics: true },
        paragraph: { indent: { left: 720, right: 360 }, spacing: { before: 120, after: 120 },
          border: { left: { style: BorderStyle.SINGLE, size: 14, color: GOLD, space: 14 } }}},
      { id: "Caption", name: "Caption", basedOn: "Normal",
        run: { font: "Calibri", size: 18, italics: true, color: MGRAY },
        paragraph: { alignment: AlignmentType.CENTER, spacing: { before: 60, after: 180 } }},
      { id: "TOC1", name: "TOC 1", basedOn: "Normal",
        run: { font: "Calibri", size: 22, bold: true, color: NAVY },
        paragraph: { spacing: { before: 120, after: 40 } }},
      { id: "TOC2", name: "TOC 2", basedOn: "Normal",
        run: { font: "Calibri", size: 20, color: BLACK },
        paragraph: { indent: { left: 360 }, spacing: { before: 40, after: 40 } }},
      { id: "TOC3", name: "TOC 3", basedOn: "Normal",
        run: { font: "Calibri", size: 18, color: MGRAY },
        paragraph: { indent: { left: 720 }, spacing: { before: 20, after: 20 } }},
    ]
  },
  sections: [{
    properties: {
      page: { size: { width: 11906, height: 16838 },
               margin: { top: 1440, right: 1134, bottom: 1440, left: 1134 } }
    },
    headers: {
      default: new Header({ children: [
        new Paragraph({
          children: [
            new TextRun({ text: "GHS-Panel Nigeria \u2013 Wave 4 (2018/2019)\t", font: "Calibri", size: 16, color: MGRAY }),
            new PageNumberElement({ font: "Calibri", size: 16, color: MGRAY })
          ],
          tabStops: [{ type: TabStopType.RIGHT, position: TabStopPosition.MAX }],
          border: { bottom: { style: BorderStyle.SINGLE, size: 4, color: GOLD, space: 4 } },
          spacing: { after: 0 }
        })
      ]})
    },
    footers: {
      default: new Footer({ children: [
        new Paragraph({
          children: [new TextRun({
            text: "Cheikh Thioub | Oumarou Firhoun \u2014 ENSAE ISE 1 \u2014 Avril 2025",
            font: "Calibri", size: 16, color: MGRAY
          })],
          border: { top: { style: BorderStyle.SINGLE, size: 4, color: LGRAY, space: 4 } },
          spacing: { before: 0 }
        })
      ]})
    },
    children: [new Paragraph({ children: [new TextRun(".")], style: "Normal" })]
  }]
});

Packer.toBuffer(doc).then(buf => {
  fs.writeFileSync('reference.docx', buf);
  console.log('OK');
});
