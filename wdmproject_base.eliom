{shared{
  open Eliom_content.Html5.F
  open Eliom_content.Html5
}}

let application_name = "wdmproject"

let () = Eba_db.init ~port:3000 ~database:"wdmproject" ()

let () = Eba_email.set_mailer "/usr/sbin/sendmail"

module App = Eliom_registration.App(struct
    let application_name = application_name
  end)
