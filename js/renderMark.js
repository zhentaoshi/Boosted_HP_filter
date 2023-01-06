function getMd(url) {
  let rendererMD = new marked.Renderer();
  $.ajax({
    type: 'get',
    url: url,
    async: false,
    dataType: "html",
    success: function (res) {
      marked.setOptions({
        renderer: rendererMD,
        gfm: true,
        tables: true,
        breaks: false,
        pedantic: false,
        sanitize: false,
        smartLists: true,
        smartypants: false,
        highlight: function (code) {
          return hljs.highlightAuto(code).value;
        }
      });
  
      document.getElementById('content').innerHTML = marked.parse(res);
      //在文档加载后为元素添加属性
      document.querySelectorAll('table').forEach(function (el) {
        el.setAttribute('border', '1')
      })
      document.querySelectorAll('code').forEach(function (el) {
        //缺少这个类代码块没有背景
        el.classList.add('hljs')
      })

      // a标签新窗口打开
      $('#content a').attr('target','_blank')
    }
  })
}
