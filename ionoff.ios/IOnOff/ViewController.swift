//
//  ViewController.swift
//  LocalWKWebView
//
//  Created by Nikolay Dimolarov on 08.02.18.
//  Copyright © 2018 Nikolay Dimolarov. All rights reserved.
//

import UIKit
import WebKit

class ViewController: UIViewController, WKNavigationDelegate {
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        // enable JavaScript via config
        let preferences = WKPreferences()
        preferences.javaScriptEnabled = true
        let configuration = WKWebViewConfiguration()
        configuration.websiteDataStore = WKWebsiteDataStore.default()
        configuration.preferences = preferences
        
        // setup webView with config and load local HTML file
        let webView = WKWebView(frame: view.bounds, configuration: configuration)
        let htmlPath = Bundle.main.path(forResource: "index", ofType: "html", inDirectory: "www")
        let folderPath = Bundle.main.bundlePath
        let baseUrl = URL(fileURLWithPath: folderPath, isDirectory: true)
        do {
            let htmlString = try NSString(contentsOfFile: htmlPath!, encoding: String.Encoding.utf8.rawValue)
            webView.loadHTMLString(htmlString as String, baseURL: baseUrl)
        } catch {
            // catch error
        }
        webView.navigationDelegate = self
        view = webView
    }
    
    /*
     Alternative solution with webView.loadFileURL():
     
     override func viewDidLoad() {
     super.viewDidLoad()
     let webView = WKWebView()
     let htmlPath = Bundle.main.path(forResource: "index", ofType: "html")
     let htmlUrl = URL(fileURLWithPath: htmlPath!, isDirectory: false)
     webView.loadFileURL(htmlUrl, allowingReadAccessTo: htmlUrl)
     webView.navigationDelegate = self
     view = webView
     }
     */
    
    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }
}
